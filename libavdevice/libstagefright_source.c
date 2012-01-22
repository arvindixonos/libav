/*
 * Interface to Android capture devices via libstagefright
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

#include "libavutil/opt.h"

#include "libstagefright_source.h"

typedef struct
{
    AVClass *class;
    AudioSourceContext ctx;
} AudioSourceInfo;

typedef struct
{
    AVClass *class;
    CameraSourceContext ctx;
} CameraSourceInfo;

static int audio_read_header(AVFormatContext *s1, AVFormatParameters *ap)
{
    AudioSourceContext *ctx = s1->priv_data;
    AVStream *st;
    int ret;

    st = av_new_stream(s1, 0);
    if (!st)
        return AVERROR(ENOMEM);

    ctx->fmt_ctx = s1;
    ret = sf_audio_init(ctx);
    if (ret != 0) {
        av_log(s1, AV_LOG_ERROR, "sf_audio_init failed: %d\n", ret);
        return AVERROR(ret);
    }
    /* take real parameters */
    st->codec->codec_type  = AVMEDIA_TYPE_AUDIO;
    st->codec->codec_id    = ctx->codec_id;
    st->codec->sample_rate = ctx->sample_rate;
    st->codec->channels    = ctx->channels;
    av_set_pts_info(st, 64, 1, 1000000);  /* 64 bits pts in us */
    return 0;
}

static int audio_read_packet(AVFormatContext *s1, AVPacket *pkt)
{
    AudioSourceContext *ctx = s1->priv_data;

    return sf_read_packet(s1, ctx->src_ctx, pkt);
}

static int audio_read_close(AVFormatContext *s1)
{
    AudioSourceContext *ctx = s1->priv_data;
    sf_audio_destroy(ctx);
    return 0;
}

static int camera_read_header(AVFormatContext *s1, AVFormatParameters *ap)
{
    CameraSourceContext *ctx = s1->priv_data;
    AVStream *st;
    int ret;

    st = av_new_stream(s1, 0);
    if (!st)
        return AVERROR(ENOMEM);

    ctx->fmt_ctx = s1;
    ret = sf_camera_init(ctx);
    if (ret != 0) {
        av_log(s1, AV_LOG_ERROR, "sf_audio_init failed: %d\n", ret);
        return AVERROR(ret);
    }
    /* take real parameters */
    st->codec->codec_type = AVMEDIA_TYPE_VIDEO;
    st->codec->codec_id = ctx->codec_id;
    st->codec->width  = ctx->width;
    st->codec->height = ctx->height;
    st->codec->pix_fmt = ctx->pix_fmt;
    st->codec->time_base = ctx->time_base;
    st->codec->bit_rate = ctx->frame_size * 1/av_q2d(ctx->time_base) * 8;
    av_set_pts_info(st, 64, 1, 1000000);  /* 64 bits pts in us */
    return 0;
}

static int camera_read_packet(AVFormatContext *s1, AVPacket *pkt)
{
    CameraSourceContext *ctx = s1->priv_data;

    return sf_read_packet(s1, ctx->src_ctx, pkt);
}

static int camera_read_close(AVFormatContext *s1)
{
    CameraSourceContext *ctx = s1->priv_data;
    sf_camera_destroy(ctx);
    return 0;
}

#ifdef CONFIG_LIBSTAGEFRIGHT_AUDIO_INDEV
static const AVOption options[] = {
    { "device_idx", " Source device index",
      offsetof(AudioSourceContext, device_idx), FF_OPT_TYPE_INT, {.dbl = 1}, 0, INT_MAX, AV_OPT_FLAG_DECODING_PARAM },
    { "sample_rate", "",
      offsetof(AudioSourceContext, sample_rate), FF_OPT_TYPE_INT, {.dbl = 48000}, 1, INT_MAX, AV_OPT_FLAG_DECODING_PARAM },
    { "channels", "", offsetof(AudioSourceContext, channels), FF_OPT_TYPE_INT, {.dbl = 2},     1, 2, AV_OPT_FLAG_DECODING_PARAM },
    { NULL },
};

static const AVClass aasrc_demuxer_class = {
    .class_name     = "android audio demuxer",
    .item_name      = av_default_item_name,
    .option         = options,
    .version        = LIBAVUTIL_VERSION_INT,
};

AVInputFormat ff_libstagefright_audio_demuxer = {
    "lsfaudio",
    NULL_IF_CONFIG_SMALL("Android audio input source via libstagefright"),
    sizeof(AudioSourceInfo),
    NULL,
    audio_read_header,
    audio_read_packet,
    audio_read_close,
    .flags = AVFMT_NOFILE,
    .priv_class = &aasrc_demuxer_class,
};
#endif

#ifdef CONFIG_LIBSTAGEFRIGHT_CAMERA_INDEV
static const AVOption camera_options[] = {
    { "device_idx", " Source device index",
      offsetof(CameraSourceContext, device_idx), FF_OPT_TYPE_INT, {.dbl = 1}, 0, INT_MAX, AV_OPT_FLAG_DECODING_PARAM },
    { "preview_enable", "Enable preview surface",
      offsetof(CameraSourceContext, preview_enable), FF_OPT_TYPE_INT, {.dbl = 1},     0, 1, AV_OPT_FLAG_DECODING_PARAM },
    { "preview_width", "",
      offsetof(CameraSourceContext, pv_width), FF_OPT_TYPE_INT, {.dbl = 320}, 1, INT_MAX, AV_OPT_FLAG_DECODING_PARAM },
    { "preview_height", "",
      offsetof(CameraSourceContext, pv_height), FF_OPT_TYPE_INT, {.dbl = 240}, 1, INT_MAX, AV_OPT_FLAG_DECODING_PARAM },
    { NULL },
};

static const AVClass camera_demuxer_class = {
    .class_name     = "android camera demuxer",
    .item_name      = av_default_item_name,
    .option         = camera_options,
    .version        = LIBAVUTIL_VERSION_INT,
};

AVInputFormat ff_libstagefright_camera_demuxer = {
    "lsfcamera",
    NULL_IF_CONFIG_SMALL("Android video camera input source via libstagefright"),
    sizeof(CameraSourceInfo),
    NULL,
    camera_read_header,
    camera_read_packet,
    camera_read_close,
    .flags = AVFMT_NOFILE,
    .priv_class = &camera_demuxer_class,
};
#endif
