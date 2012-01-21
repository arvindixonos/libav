/*
 * Interface to Android capture devices through libstagefright
 *
 * Copyright (C) 2012 Dmitry Monakhov
 *
 * This file is part of Libav.
 *
 * Libav is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Libav is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Libav; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <utils/Errors.h>
#include <binder/ProcessState.h>
#include <media/stagefright/MediaDebug.h>
#include <media/stagefright/MediaDefs.h>
#include <media/stagefright/MetaData.h>
#include <media/stagefright/MediaBuffer.h>
#include <media/stagefright/AudioSource.h>
#include <new>

#include "libstagefright_source.h"

using namespace android;

/* TODO It is reasonable to move this function to some common header */
static int ERR_ACODE(int code)
{
    switch (code) {
    case android::OK:
        return 0;
    case android::NO_MEMORY:
        return ENOMEM;
    case android::INVALID_OPERATION:
        return ENOSYS;
    case android::NAME_NOT_FOUND:
        return ENOENT;
    case android::PERMISSION_DENIED:
        return EPERM;
    case android::NO_INIT:
        return ENODEV;
    case android::ALREADY_EXISTS:
        return EEXIST;
    case android::DEAD_OBJECT:
        return EPIPE;
    default:
        return EINVAL;
    }
    return EINVAL;
}

typedef struct
{
    sp<MediaSource> source;
    int64_t start_ts;
    unsigned int init:1;	/* Source was successully inited */
    unsigned int started:1;	/* Source was successully started */
    unsigned int copy_mem:1;	/* Copy packet's data required */
} MediaSourceContext;

static void sf_source_destroy(AVFormatContext *s, MediaSourceContext *src_ctx)
{
    status_t err = OK;
    if (!src_ctx)
        return;
    if (src_ctx->started) {
        err = src_ctx->source->stop();
        if (err != OK)
            av_log(s, AV_LOG_ERROR, "MediaSoure->stop() failed with: %d\n", err);
    }
    delete src_ctx; /* src_ctx->source will be destroyed inside destryctor */

}

int sf_audio_init(AudioSourceContext *ctx)
{
    MediaSourceContext *src_ctx = (MediaSourceContext*) ctx->src_ctx;
    status_t err;
    if (src_ctx != NULL) {
        av_log(ctx->fmt_ctx, AV_LOG_ERROR,
               "AudioSource context is already initialized\n");
        return EBUSY;
    }
    src_ctx = new MediaSourceContext();
    src_ctx->source = new AudioSource(ctx->device_idx, ctx->sample_rate,
                                  ctx->channels);

    err = ((AudioSource*)src_ctx->source.get())->initCheck();
    if (err != OK) {
        av_log(ctx->fmt_ctx, AV_LOG_ERROR, "audio source is not initialize\n");
        return ERR_ACODE(err);
    }
    src_ctx->init = 1;
    /* AudioSource use only one internal buffer, so we have to copy it in order
     * to avoid buffer starvation */
    src_ctx->copy_mem = 1;
    ctx->src_ctx = src_ctx;
    /* libstagefright returns int16_t */
#if HAVE_BIGENDIAN
    ctx->codec_id = CODEC_ID_PCM_S16BE;
#else
    ctx->codec_id = CODEC_ID_PCM_S16LE;
#endif
    return 0;
}

void sf_pkt_destruct(struct AVPacket *pkt)
{
    MediaBuffer *buffer = (MediaBuffer*)pkt->priv;
    if (buffer == NULL)
        return;
    pkt->priv = NULL;
    pkt->size = 0;
    buffer->release();
}

int sf_read_packet(AVFormatContext *s, void *sc, AVPacket *pkt)
{
    int64_t pts;
    status_t err;
    MediaBuffer *buffer;
    MediaSourceContext *src_ctx = (MediaSourceContext*) sc;

    if (!src_ctx || !src_ctx->source.get() || !src_ctx->init) {
        av_log(s, AV_LOG_ERROR, "MediaSource context not initialized\n");
        return EINVAL;
    }

    if (!src_ctx->started) {
        err = src_ctx->source->start();
        if (err != OK) {
            av_log(s, AV_LOG_ERROR, "MediaSource->start() failed with: %d\n", err);
            return ERR_ACODE(err);
        }
        src_ctx->started = 1;
        src_ctx->start_ts = av_gettime();
    }
    err = src_ctx->source->read(&buffer);
    if (err != OK)
        goto fail;

    if(!(buffer->meta_data()->findInt64(kKeyTime, &pts))) {
        err = NAME_NOT_FOUND;
        goto fail;
    }
    if (src_ctx->copy_mem) {
        if (av_new_packet(pkt, buffer->range_length()) < 0) {
            err = NO_MEMORY;
            goto fail;
        }
        memcpy(pkt->data, buffer->data(), pkt->size);
        buffer->release();
    } else {
        pkt->data = (uint8_t*)buffer->data();
        pkt->priv = buffer;
        pkt->destruct = sf_pkt_destruct;
    }
    pkt->size = buffer->range_length();
    pkt->pts = src_ctx->start_ts + pts;
    return pkt->size;
fail:
    pkt->size = 0;
    av_log(s, AV_LOG_ERROR, "Read failed with %d\n", err);
    av_free_packet(pkt);
    if (buffer)
        buffer->release();

    return -ERR_ACODE(err);
}

void sf_audio_destroy(AudioSourceContext *ctx)
{
    MediaSourceContext *src_ctx = (MediaSourceContext*) ctx->src_ctx;

    if (!src_ctx) {
        sf_source_destroy(ctx->fmt_ctx, src_ctx);
        ctx->src_ctx = NULL;
    }
}
