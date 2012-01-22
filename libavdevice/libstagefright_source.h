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

#ifndef AVDEVICE_LIBSTAGEFRIGHT_SOURCE_H
#define AVDEVICE_LIBSTAGEFRIGHT_SOURCE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "libavutil/log.h"
#include "libavformat/avformat.h"

    typedef struct {
        AVFormatContext *fmt_ctx;
        uint32_t device_idx;
        uint32_t sample_rate;
        uint32_t channels;
        enum CodecID codec_id;
        void *src_ctx;	 		/* MediaSourceCtx struct */
    } AudioSourceContext;

    typedef struct {
        char *data;
        size_t size;
    } ParcelInfo;
    typedef struct {
        AVFormatContext *fmt_ctx;
        uint32_t device_idx;
        uint32_t height;
        uint32_t width;
        uint32_t pv_height;
        uint32_t pv_width;
        ParcelInfo pv_parcel;
        enum CodecID codec_id;
        enum PixelFormat pix_fmt;
        uint32_t frame_size;
        AVRational time_base;
        void *src_ctx;	 		/* MediaSourceCtx struct */
        void *cam_ctx;
        unsigned int preview_enable;
    } CameraSourceContext;

    int sf_audio_init(AudioSourceContext *c);
    int sf_read_packet(AVFormatContext *s, void *src_ctx,  AVPacket *pkt);
    void sf_audio_destroy(AudioSourceContext *c);
    int sf_camera_init(CameraSourceContext *c);
    void sf_camera_destroy(CameraSourceContext *c);
#ifdef __cplusplus
}
#endif

#endif /* AVDEVICE_LIBSTAGEFRIGHT_SOURCE_H */
