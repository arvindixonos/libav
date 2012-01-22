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
#include <binder/IPCThreadState.h>
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

/*
 * XXX: By unknown reason standard CameraSource  returns strange buffers
 * from CameraSource::read() method (buffers with size of 16 bytes in my case).
 * Seems this is happens due to incorrect initialization, but i dont know how
 * to fix it. From other point of view camera preview callback interface works
 * perfect, so let's use it.
 * It is fun but seems like i'm not an only one who dont know how to use recording
 * interface, OpenCV and Bambuser use same trick instead of standard camera
 * recording interface.
 * 								-dmonakhov@
 */
#define USE_CAMERA_PREVIEW_INTERFACE 1

#ifdef USE_CAMERA_PREVIEW_INTERFACE

#define  ASSERT_MUTEX_IS_LOCKED(_mtx_) \
    if (!(_mtx_)->tryLock()) \
        av_log(NULL,AV_LOG_PANIC, "%s:%d Mutex was unlocked\n", __FILE__, __LINE__);

/*
 * Queue class with external common lock.
 * XXX: Android MediaBuffer interface is awful, nextBuffer is available only
 * for MediaGroupBuffer, let's pretend that our clas is indeed MediaGroupBuffer
 */
namespace android {
class MediaBufferGroup {
public:
    MediaBufferGroup(Mutex *lk): lock(lk), head(0), tail(0), is_alive(1) {}

    bool empty() {return head == NULL;}
    void setAlive(int alive) {
        is_alive = alive;
        condition.signal();
    }
    void push(MediaBuffer *buffer) {
        ASSERT_MUTEX_IS_LOCKED(lock);
        if (head) {
            tail->setNextBuffer(buffer);
        } else {
            head = buffer;
            condition.signal();
        }
        buffer->setNextBuffer(NULL);
        tail = buffer;
    }
    MediaBuffer* pop(int may_block) {
        ASSERT_MUTEX_IS_LOCKED(lock);
        MediaBuffer* buffer = NULL;
        while(is_alive) {
            buffer = head;
            if (head) {
                head = head->nextBuffer();
                break;
            } else if (!may_block)
                break;
            condition.wait(*lock);
        }
        return buffer;
    }
    status_t erase(MediaBuffer *buffer) {
        ASSERT_MUTEX_IS_LOCKED(lock);
        MediaBuffer *p, *prev = head;
        if (!head)
            return android::NAME_NOT_FOUND;
        if (head == buffer) {
            head = head->nextBuffer();
            return OK;
        }
        p = head->nextBuffer();
        while(p) {
            if (p == buffer) {
                if (p == tail)
                    tail = prev;
                prev->setNextBuffer(p->nextBuffer());
                return OK;
            }
            prev = p;
            p = p->nextBuffer();
        }
        return android::NAME_NOT_FOUND;
    }
    ~MediaBufferGroup() {
        MediaBuffer *buffer;
        while ((buffer = head)) {
            head = head->nextBuffer();
            av_log(NULL, AV_LOG_DEBUG,
                   "Destroy buffer:(%p) {data:%p ref:%d}\n", buffer,
                   buffer->data(), buffer->refcount());
            buffer->setObserver(NULL);
            delete buffer;
        }
    }
private:
    Mutex *lock;
    Condition condition;
    MediaBuffer *head, *tail;
    unsigned is_alive;
};
};
class CameraPreviewSource : public MediaSource, public MediaBufferObserver {
public:
    CameraPreviewSource(const sp<Camera> &camera, void *avctx);

    virtual ~CameraPreviewSource();
    virtual status_t start(MetaData *params = NULL);
    virtual status_t stop();
    virtual sp<MetaData> getFormat() { return mMeta; }
    virtual status_t read(MediaBuffer **buffer,
                          const ReadOptions *options = NULL);
    virtual void signalBufferReturned(MediaBuffer* buffer);
private:
    friend class PreviewListener;

    sp<Camera> mCamera;
    sp<MetaData> mMeta;
    void *mAvCtx;
    Mutex mLock;
    MediaBufferGroup unused_queue; /* Buffers for incoming frames */
    MediaBufferGroup ready_queue;  /* Buffers for ::read() request */
    MediaBufferGroup busy_queue;   /* Buffers belongs to user, not yet released */
    int64_t start_time;
    int32_t mNumReceived;
    int32_t mNumEncoded;
    int32_t mNumDropped;
    int32_t mNumFrames;
    bool mStarted;
    void postData(int32_t msgType, const sp<IMemory> &data);
    CameraPreviewSource(const CameraPreviewSource &);
    CameraPreviewSource &operator=(const CameraPreviewSource &);
};

struct PreviewListener : public CameraListener {
    PreviewListener(const sp<CameraPreviewSource> &source): mSource(source) {}

    virtual void notify(int32_t msgType, int32_t e1, int32_t e2);
    virtual void postData(int32_t msgType, const sp<IMemory> &dataPtr);
    virtual void postDataTimestamp(
            nsecs_t timestamp, int32_t msgType, const sp<IMemory>& dataPtr);
protected:
    virtual ~PreviewListener() {};

private:
    wp<CameraPreviewSource> mSource;

    PreviewListener(const PreviewListener &);
    PreviewListener &operator=(const PreviewListener &);
};

void PreviewListener::notify(int32_t msgType, int32_t a1, int32_t a2)
{
    av_log(NULL, AV_LOG_DEBUG, "PreviewListener::notify(%d, %d, %d)",
           msgType, a1, a2);
}

void PreviewListener::postData(int32_t msgType, const sp<IMemory> &dp)
{
    sp<CameraPreviewSource> source = mSource.promote();
    if (source.get() != NULL)
        source->postData(msgType, dp);
}

void PreviewListener::postDataTimestamp(nsecs_t timestamp, int32_t msgType,
                                        const sp<IMemory>& frame) {
    sp<CameraPreviewSource> source = mSource.promote();
    LOGV("%s src:%p dp{%p,%d}\n", __FUNCTION__, source.get(), frame->pointer(),
         frame->size());
    if (source.get() && source->mCamera.get()) {
        int64_t toc = IPCThreadState::self()->clearCallingIdentity();
        source->mCamera->releaseRecordingFrame(frame);
        IPCThreadState::self()->restoreCallingIdentity(toc);
    } else {
        /* Should never happen */
        av_log(NULL, AV_LOG_ERROR,
               "Release context absent, IMemory{ptr:%p, sz:%d} will leak",
               frame->pointer(), frame->size());
    }
}

CameraPreviewSource::CameraPreviewSource(const sp<Camera> &camera, void *avctx)
    : mCamera(camera),
      mAvCtx(avctx),
      mLock(),
      unused_queue(&mLock),
      ready_queue(&mLock),
      busy_queue(&mLock),
      mNumReceived(0),
      mNumEncoded(0),
      mNumDropped(0),
      mNumFrames(50),
      mStarted(false)
{

    int64_t ipc_toc = IPCThreadState::self()->clearCallingIdentity();
    String8 s = mCamera->getParameters();
    IPCThreadState::self()->restoreCallingIdentity(ipc_toc);

    int32_t width, height;
    CameraParameters params(s);
    params.getPreviewSize(&width, &height);

    const char *str_pix_fmt = params.get(CameraParameters::KEY_PREVIEW_FORMAT);
    int32_t pix_fmt = OMX_COLOR_FormatYUV420SemiPlanar;
    if (str_pix_fmt) {
        if (!strcmp(str_pix_fmt, CameraParameters::PIXEL_FORMAT_YUV420SP))
            pix_fmt = OMX_COLOR_FormatYUV420SemiPlanar;
        else if (!strcmp(str_pix_fmt, CameraParameters::PIXEL_FORMAT_YUV422SP))
            pix_fmt = OMX_COLOR_FormatYUV422SemiPlanar;
        else if (!strcmp(str_pix_fmt, CameraParameters::PIXEL_FORMAT_RGB565))
            pix_fmt = OMX_COLOR_Format16bitRGB565;
        else if (!strcmp(str_pix_fmt, CameraParameters::PIXEL_FORMAT_YUV422I))
            pix_fmt =  OMX_COLOR_FormatYCbYCr;
        else
            av_log(mAvCtx, AV_LOG_ERROR, "Unknown frame format (%s)\n",
                   str_pix_fmt);
    }
    mMeta = new MetaData;
    mMeta->setCString(kKeyMIMEType, MEDIA_MIMETYPE_VIDEO_RAW);
    mMeta->setInt32(kKeyColorFormat, pix_fmt);
    mMeta->setInt32(kKeyWidth, width);
    mMeta->setInt32(kKeyHeight, height);
    mMeta->setInt32(kKeyStride, width);
    mMeta->setInt32(kKeySliceHeight, height);

    int32_t frame_size  = (width * height * 3) / 2;
    /* Lock mutex in order to satisfy queue's locking rules */
    Mutex::Autolock autoLock(mLock);
    for (int num = 0; num < mNumFrames; num++) {
        MediaBuffer *buffer = new MediaBuffer(frame_size);
        buffer->setObserver(this);
        av_log(NULL, AV_LOG_DEBUG, "Alloc buffer:(%p) {data:%p ref:%d}\n", buffer,
                   buffer->data(), buffer->refcount());
        unused_queue.push(buffer);
    }
}

CameraPreviewSource::~CameraPreviewSource()
{
    if (mStarted) {
        stop();
    }
    /* Buffers will be destroyed by queues destructors */
}
status_t CameraPreviewSource::start(MetaData *meta)
{
    Mutex::Autolock autoLock(mLock);
    if (mStarted)
        return OK;
    av_log(mAvCtx, AV_LOG_VERBOSE, "Start\n");
    start_time = av_gettime();
    int64_t token = IPCThreadState::self()->clearCallingIdentity();
    mCamera->setListener(new PreviewListener(this));
    mCamera->setPreviewCallbackFlags(FRAME_CALLBACK_FLAG_ENABLE_MASK);
    IPCThreadState::self()->restoreCallingIdentity(token);
    mStarted = true;
    ready_queue.setAlive(1);

    return OK;
}

status_t CameraPreviewSource::stop() {
    MediaBuffer *buffer;
    Mutex::Autolock autoLock(mLock);
    if (!mStarted)
        return OK;
    av_log(mAvCtx, AV_LOG_VERBOSE, "Stop\n");
    int64_t token = IPCThreadState::self()->clearCallingIdentity();
    mCamera->setPreviewCallbackFlags(0);
    mCamera->setListener(NULL);
    IPCThreadState::self()->restoreCallingIdentity(token);

    mStarted = false;
    /* After that moment no one can add new buffers to ready_queue
     * Drain all buffers to temroral queue. */
    MediaBufferGroup tmp_queue(&mLock);
    while (!ready_queue.empty()) {
        buffer = ready_queue.pop(1);
        tmp_queue.push(buffer);
    }
    /* Notify users which may be blocked on ready_queue that queue was stopeed */
    ready_queue.setAlive(0);
    while (!busy_queue.empty()) {
        av_log(mAvCtx, AV_LOG_DEBUG, "Waiting for outstanding buffers: %d",
               busy_queue.empty());
        /* Released buffers will be pushed back to unsuded_queue */
        buffer = unused_queue.pop(1);
        tmp_queue.push(buffer);
    }
    /* Return buffers back to unused queue, so later start() can use it*/
    while (!tmp_queue.empty()) {
        buffer = tmp_queue.pop(1);
        unused_queue.push(buffer);
    }
    av_log(mAvCtx, AV_LOG_DEBUG, "Queues unused/ready/busy %d/%d/%d\n",
           unused_queue.empty(), ready_queue.empty(), busy_queue.empty());
    av_log(mAvCtx, AV_LOG_VERBOSE, "Frames received/encoded/dropped: "
           "%d/%d/%d in %lld us",
           mNumReceived, mNumEncoded, mNumDropped,
           av_gettime() - start_time);
    return OK;
}

void CameraPreviewSource::signalBufferReturned(MediaBuffer *buffer) {
    Mutex::Autolock autoLock(mLock);
    av_log(mAvCtx, AV_LOG_DEBUG, "signalBufferReturned: %p", buffer->data());
    if (busy_queue.erase(buffer) != OK) {
        av_log(mAvCtx, AV_LOG_ERROR, "signalBufferReturned: bogus buffer{%p,%d}",
               buffer->data(), buffer->refcount());
        return;
    }
    unused_queue.push(buffer);
}

status_t CameraPreviewSource::read(MediaBuffer **buffer, const ReadOptions *op)
{
    Mutex::Autolock autoLock(mLock);
    if (!mStarted)
        return OK;
    av_log(mAvCtx, AV_LOG_DEBUG, "read\n");
    *buffer = ready_queue.pop(1);
    if (*buffer) {
        mNumEncoded++;
        (*buffer)->add_ref();
        busy_queue.push(*buffer);
    }
    return OK;
}

void CameraPreviewSource::postData(int32_t msgType, const sp<IMemory> &data)
{
//    LOGV("%s started:%d dp{%p,%d}\n", __FUNCTION__, mStarted, data->pointer(), data->size());
    if (!mStarted)
        return;
    int64_t timestampUs = av_gettime();

    Mutex::Autolock autoLock(mLock);
    av_log(mAvCtx, AV_LOG_DEBUG, "postData: timestamp %lld us",
           timestampUs - start_time);
    if (!mStarted)
        return;
    mNumReceived++;
    MediaBuffer *buffer = unused_queue.pop(0);
    if (!buffer) {
        av_log(mAvCtx, AV_LOG_VERBOSE, "Failed to get unused buffer, will drop\n");
        mNumDropped++;
        return;
    }
    assert(buffer->size() == data->size());
    memcpy(buffer->data(), data->pointer(), data->size());
    buffer->set_range(0, data->size());
    buffer->meta_data()->clear();
    buffer->meta_data()->setInt64(kKeyTime, timestampUs - start_time);
    ready_queue.push(buffer);
}

#endif /* USE_CAMERA_PREVIEW_INTERFACE */

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
    ProcessState::self()->startThreadPool();
    cam_ctx = new CameraContext();
    /* User want to use explicit surface as a preview display */
    if (ctx->pv_parcel.size != 0) {
        void* raw;
        parcel.setDataSize(ctx->pv_parcel.size);
        parcel.setDataPosition(0);
        raw = parcel.writeInplace(ctx->pv_parcel.size);
        memcpy(raw, (ctx->pv_parcel.data), ctx->pv_parcel.size);
        /*
         * TODO: Following line refused to work for me by unknown reason
         * when compiled via standard Makefile, but it is works when built same
         * code via $NDK/ndk-buil script. Let's hope that compile script will
         * be fixed some day.
         */
        cam_ctx->pv_surface = Surface::readFromParcel(parcel);
        if (!cam_ctx->pv_surface.get())
            goto fail;
    }

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
#ifdef USE_CAMERA_PREVIEW_INTERFACE
    src_ctx->source = new CameraPreviewSource(cam_ctx->camera, ctx->fmt_ctx);
    src_ctx->copy_mem = 0;
#else /* Use standard CameraSource interface */
    src_ctx->source = android::CameraSource::CreateFromCamera(cam_ctx->camera);
    src_ctx->copy_mem = 1;
#endif
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
