#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sndfile.h>

struct _t
{
    char error_message[255];
};

typedef struct _t t;

static char* copy_string (char* dst, const char* src, size_t max_length)
{
    strncpy(dst, src, max_length);
    if (max_length > 0)
    {
        dst[max_length - 1] = 0;
    }
    return dst;
}

static void set_error_message (t* out, const char* str)
{
    copy_string(out->error_message, str, sizeof(out->error_message));
}

static const char* ok = "";

void unused_foo (t* out)
{
    set_error_message(out, ok);
}

typedef void debug_print_t (char* message);

void debug_print_to_stdout (char* message)
{
    fputs("DEBUG: ", stdout);
    fputs(message, stdout);
    fputs("\n", stdout);
}

/**
Read the audio samples in the 1-channel 44100-samples-per-second file
into an array of 16-bit two's complement signed integers.

@param debug_print
NULL or debug_print_to_stdout

@param[out] err_msg
a null-terminated string;
contains the empty string iff the operation is successful;
contains the error message otherwise.
If err_max is too small, the message will be truncated.

@param err_max
The maximum size of the err_msg char array,
including the null terminator.

@param[out] buffer
should be big enough.
Before reading the contents of the buffer,
you must make sure that err_msg has length zero
(that is err_msg[0] == 0).

@param buf_max
maximum number of bytes the buffer can contain

@param file_path
file system path of the input file
*/
void slurp_file_s16
(
    debug_print_t debug_print
    , char* err_msg
    , size_t err_max
    , short* buffer
    , size_t buf_max
    , const char* file_path
)
{
    assert(sizeof(short) == 2);

    SF_INFO info = { 0 };
    SNDFILE* handle = sf_open(file_path, SFM_READ, &info);
    if (handle == NULL) { goto open_error; }

    const int supported_channel = 1;
    const int supported_sample_rate = 44100;
    const int channel = info.channels;
    const int sample_rate = info.samplerate;
    const sf_count_t expected_frame_count = info.frames;
    assert(expected_frame_count >= 0);
    if (channel != supported_channel) { goto channel_error; }
    if (sample_rate != supported_sample_rate) { goto sample_rate_error; }

    const sf_count_t sample_size = sizeof(*buffer);
    const sf_count_t frame_size = channel * sample_size;
    const sf_count_t max_frame_count = buf_max / frame_size;
    const size_t max_sample_count = buf_max / sample_size;
    if (expected_frame_count > max_frame_count) { goto buffer_overflow; }

    const sf_count_t actual_frame_count = sf_readf_short(handle, buffer, max_frame_count);
    if (actual_frame_count != expected_frame_count) { goto read_error; }
    const size_t actual_sample_count = actual_frame_count * channel;
    const size_t zero_sample_count = max_sample_count - actual_sample_count;
    memset(buffer + actual_sample_count, 0, sample_size * zero_sample_count);

    if (debug_print != NULL) {
        char message[1024];
        snprintf(message, sizeof(message),
            "slurp_file_s16: ok: "
            "libsndfile-format 0x%x channel %d "
            "sample-rate %d frame %" PRId64 " "
            "path \"%s\""
            , info.format, channel, sample_rate, actual_frame_count, file_path
        );
        debug_print(message);
    }

    snprintf(err_msg, err_max, "%s", "");

end:

    if (handle != NULL) {
        if (sf_close(handle) != 0) {
            snprintf(err_msg, err_max
                , "error: sf_close: %s: %s", file_path, sf_strerror(handle)
            );
        }
    }

    return;

open_error:

    snprintf(err_msg, err_max, "error: sf_open: %s: %s", file_path, sf_strerror(handle));
    goto end;

channel_error:

    snprintf(err_msg, err_max,
        "error: %s has %d channels; only %d-channel audio sample files are supported. "
        "Use another file, or convert the file with another program."
        , file_path, channel, supported_channel
    );
    goto end;

sample_rate_error:

    snprintf(err_msg, err_max,
        "error: the sample rate of %s is %d; the only supported sample rate is %d. "
        "Use another file, or convert the file with another program."
        , file_path, sample_rate, supported_sample_rate
    );
    goto end;

buffer_overflow:

    snprintf(err_msg, err_max,
        "error: slurp: the %zu-byte buffer is too small for the %" PRId64 "-frame file '%s'. "
        "Use another file, downsample the file with another program, "
        "or change the source code and restart this program."
        , buf_max, expected_frame_count, file_path
    );
    goto end;

read_error:

    snprintf(err_msg, err_max,
        "error: sf_readf_short: %s: "
        "expecting to read %" PRId64 " frames, got %" PRId64 " frames. "
        "If this condition persists, it may be due to a programming error."
        , file_path, expected_frame_count, actual_frame_count
    );
    goto end;

}
