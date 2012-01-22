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
#include <binder/Parcel.h>
#include <binder/ProcessState.h>
#include <camera/ICamera.h>
#include <camera/CameraParameters.h>
#include <media/stagefright/MediaDebug.h>
#include <media/stagefright/MediaDefs.h>
#include <media/stagefright/MetaData.h>
#include <media/stagefright/MediaBuffer.h>
#include <media/stagefright/AudioSource.h>
#include <media/stagefright/CameraSource.h>
#include <media/stagefright/openmax/OMX_IVCommon.h>
#include <surfaceflinger/Surface.h>
#include <surfaceflinger/SurfaceComposerClient.h>

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

typedef struct {
    /* Preview context */
    sp<SurfaceComposerClient> 	pv_client;
    sp<SurfaceControl> 		pv_control;
    sp<Surface> 		pv_surface;
    /* Camera context */
    sp<Camera> 			camera;
    sp<ICamera> 		icamera;
////
    CameraParameters 	*params;
} CameraContext;

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

int sf_camera_init(CameraSourceContext *ctx)
{
    MediaSourceContext *src_ctx = (MediaSourceContext*) ctx->src_ctx;
    CameraContext *cam_ctx = (CameraContext *) ctx->cam_ctx;
    android::String8 params_str;
    sp<MetaData> fmt_meta;
    status_t err = UNKNOWN_ERROR;
    Parcel parcel;
    int32_t color_fmt, width, height;
    const char *mime_type;
    char tmp_str[32];

    if (src_ctx != NULL || cam_ctx != NULL) {
        av_log(ctx->fmt_ctx, AV_LOG_ERROR,
               "CameraSource context is already initialized\n");
        return EBUSY;
    }
    cam_ctx = new CameraContext();
    /* Initialize preview surface */
    if (ctx->pv_parcel.size == 0) {
        /* Create default preview surface */
        cam_ctx->pv_client = new SurfaceComposerClient();
        cam_ctx->pv_control = cam_ctx->pv_client->createSurface(
            getpid(), 0, ctx->pv_width, ctx->pv_height,
            PIXEL_FORMAT_RGB_565, 0x00000200);
        if (!cam_ctx->pv_control.get())
            goto fail;
        if (ctx->preview_enable) {
            cam_ctx->pv_client->openTransaction();
            cam_ctx->pv_control->setLayer(100000);
            cam_ctx->pv_client->closeTransaction();
        }
        parcel.setDataPosition(0);
        SurfaceControl::writeSurfaceToParcel(cam_ctx->pv_control, &parcel);
    } else {
        /* User want to use explicit surface as a preview display */
        void* raw;
        parcel.setDataSize(ctx->pv_parcel.size);
        parcel.setDataPosition(0);
        raw = parcel.writeInplace(ctx->pv_parcel.size);
        memcpy(raw, (ctx->pv_parcel.data), ctx->pv_parcel.size);
    }
    cam_ctx->pv_surface = Surface::readFromParcel(parcel);
    if (!cam_ctx->pv_surface.get())
        goto fail;

    /* Initialize camera context */
    cam_ctx->camera = android::Camera::connect(ctx->device_idx);
    if (!cam_ctx->camera.get()) {
        av_log(ctx->fmt_ctx, AV_LOG_ERROR, "Can not connect to camera :%d\n",
            ctx->device_idx);
        goto fail;
    }
    params_str = cam_ctx->camera->getParameters();
    cam_ctx->params = new android::CameraParameters(params_str);
    /* Strage old sources has no ->setVideoSize() */
    snprintf(tmp_str, sizeof(tmp_str), "%dx%d", ctx->width, ctx->height);
    cam_ctx->params->set("video-size", tmp_str);
    cam_ctx->params->setPreviewSize(ctx->pv_width, ctx->pv_height);
    err = cam_ctx->camera->setParameters(cam_ctx->params->flatten());
    if (err != OK) {
        av_log(ctx->fmt_ctx, AV_LOG_ERROR,
               "Set camera parameters failed with: %d\n", err);
        goto fail;
    }
    err = cam_ctx->camera->setPreviewDisplay(cam_ctx->pv_surface);
    if (err != OK) {
        av_log(ctx->fmt_ctx, AV_LOG_ERROR,
               "Set camera preview display failed with: %d\n", err);
        goto fail;
    }
    err = cam_ctx->camera->startPreview();
    if (err != OK) {
        av_log(ctx->fmt_ctx, AV_LOG_ERROR,
               "startPreview failed with: %d\n", err);
        goto fail;
    }
    src_ctx = new MediaSourceContext();
    src_ctx->source = android::CameraSource::CreateFromCamera(cam_ctx->camera);
    fmt_meta =  src_ctx->source->getFormat();
    if (!fmt_meta->findCString(kKeyMIMEType, &mime_type) ||
        !fmt_meta->findInt32(kKeyColorFormat, &color_fmt) ||
        !fmt_meta->findInt32(kKeyWidth, &width) ||
        !fmt_meta->findInt32(kKeyHeight, &height))
        goto fail;

    if (strcasecmp(mime_type, MEDIA_MIMETYPE_VIDEO_RAW))
        goto fail;
    ctx->codec_id = CODEC_ID_RAWVIDEO;
    /* TODO Not shure that following lines are coorect */
    av_log(ctx->fmt_ctx, AV_LOG_VERBOSE, "camera pixel format: %x\n", color_fmt);
    if (color_fmt == OMX_COLOR_FormatYUV420SemiPlanar)
        ctx->pix_fmt = PIX_FMT_NV21;
    else
        ctx->pix_fmt = PIX_FMT_YUV420P;

    ctx->width = width;
    ctx->height = height;
    /* TODO Determinate framesize from pixel format */
    ctx->frame_size  = (ctx->width * ctx->height * 3) / 2;
    ctx->time_base = (AVRational){1, 30};
    src_ctx->init = 1;
    src_ctx->copy_mem = 1;
    /* Finally attach smart pointers to context, after that context will owns
     * it's smartpointers */
    ctx->src_ctx = src_ctx;
    ctx->cam_ctx = cam_ctx;

    return 0;
fail:
    /* Nothing special should be done here because all objects allocated are owned
     * by smart pointers and will be automatically destroyed by destructors */
    if (src_ctx)
        delete src_ctx;
    if (cam_ctx) {
        if (cam_ctx->params)
            delete cam_ctx->params;
        delete cam_ctx;
    }
    return err;
}

void sf_camera_destroy(CameraSourceContext *ctx)
{
    CameraContext *cam_ctx = (CameraContext*) ctx->cam_ctx;
    MediaSourceContext *src_ctx = (MediaSourceContext*) ctx->src_ctx;

    if (!src_ctx) {
        sf_source_destroy(ctx->fmt_ctx, src_ctx);
        ctx->src_ctx = NULL;
    }
    delete cam_ctx;
    ctx->cam_ctx = NULL;
}
