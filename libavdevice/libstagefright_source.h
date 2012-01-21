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

    int sf_audio_init(AudioSourceContext *c);
    int sf_read_packet(AVFormatContext *s, void *src_ctx,  AVPacket *pkt);
    void sf_audio_destroy(AudioSourceContext *c);

#ifdef __cplusplus
}
#endif

#endif /* AVDEVICE_LIBSTAGEFRIGHT_SOURCE_H */
