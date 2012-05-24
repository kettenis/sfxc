#! /usr/bin/python

import os
import time

def vex2time(str):
    tupletime = time.strptime(str, "%Yy%jd%Hh%Mm%Ss");
    return time.mktime(tupletime)

def time2vex(secs):
    tupletime = time.gmtime(secs)
    return time.strftime("%Yy%jd%Hh%Mm%Ss", tupletime)

os.environ['TZ'] = 'UTC'
time.tzset()

class Mark4:
    def __init__(self, ntracks):
        self.ntracks = ntracks
        self.sync_size = (ntracks / 8) * 32
        self.header_size = (ntracks / 8) * 64
        self.frame_size = (ntracks / 8) * 20000
        pass

    def decode_header(self, header):
        # Treat the header as a series of BCD digits.
        bcd = 16 * [0]

        for i in xrange(16):
            for j in xrange(4):
                if ord(header[(self.ntracks / 8) * (i * 4 + j)]) & 0x01:
                    bcd[i] |= (1 << (3 - j))
                    pass
                continue
            continue
        if bcd[0] > 5:
            year = 2000 + bcd[0]
        else:
            year = 2010 + bcd[0]
            pass
        doy = bcd[1] * 100 + bcd[2] * 10 + bcd[3]
        hour = bcd[4] * 10 + bcd[5]
        min = bcd[6] * 10 + bcd[7]
        sec = bcd[8] * 10 + bcd[9]
        msec = bcd[10] * 100 + bcd[11] * 10 + bcd[12]
        t = vex2time("%dy%03dd%02dh%02dm%02ds" % (year, doy, hour, min, sec))
        return  t + msec * 1e-3

    def chunk_data(self, src, start, stop, secs_per_chunk, dest, callback, arg):
        num_chunks = int((stop - start) // secs_per_chunk)
        current_chunk = -1
        current_chunk_pos = -1
        pos = 0

        fp = open(src)
        buf = fp.read(self.frame_size + self.sync_size)
        while len(buf) > 0:
            # Search for sync word.
            offset = buf.find(self.ntracks * 4 * '\xff')
            if offset == -1:
                pos += len(buf) - self.sync_size
                buf = buf[-self.sync_size:]
                buf += fp.read(self.frame_size)
                continue

            # Found sync word.  Skip it and make sure we have a full header in
            # the remaining buffer.
            pos += offset + self.sync_size
            buf = buf[offset + self.sync_size:]
            if len(buf) < self.header_size:
                buf += fp.read(self.header_size - len(buf))
                pass

            # If we failed to read a full header, punt.
            if len(buf) < self.header_size:
                break

            secs = self.decode_header(buf)
            if secs >= stop:
                break

            if secs >= start:
                chunk = int((secs - start) // secs_per_chunk)
                if chunk != current_chunk:
                    if current_chunk_pos >= 0:
                        chunk_start = start + current_chunk * secs_per_chunk
                        chunk_stop = chunk_start + secs_per_chunk
                        callback(src, dest, current_chunk, num_chunks,
                                 chunk_start, chunk_stop, current_chunk_pos,
                                 pos - current_chunk_pos, arg)
                        pass
                    current_chunk = chunk
                    current_chunk_pos = pos
                    pass
                pass

            # Skip the frame and read the next sync word and header at the
            # position where we expect it to be.  If for some reason it
            # isn't there this will trigger a new sync word search.
            pos += self.frame_size - self.sync_size
            fp.seek(pos)
            buf = fp.read(self.sync_size + self.header_size)
            continue

        if current_chunk >= 0:
            chunk_start = start + current_chunk * secs_per_chunk
            chunk_stop = chunk_start + secs_per_chunk
            callback(src, dest, current_chunk, num_chunks,
                     chunk_start, chunk_stop, current_chunk_pos,
                     pos - current_chunk_pos, arg)
            pass

        return

    pass



class Mark5B:
    def __init__(self):
        self.sync_size = 4
        self.header_size = 12
        self.frame_size = 10016
        pass

    def decode_header(self, header):
        # Treat the header as a series of BCD digits.
        bcd = 16 * [0]

        for i in xrange(16):
            bcd[i] = (ord(header[4 + (i / 2)]) >> ((i % 2) * 4)) & 0xf
            continue
        mjd = 55000 + bcd[7] * 100 + bcd[6] * 10 + bcd[5]
        if mjd < 55500:
            mjd += 1000
            pass
        sec = bcd[4] * 10000 + bcd[3] * 1000 + bcd[2] * 100 + \
            bcd[1] * 10 + bcd[0] * 1 + bcd[15] * 0.1 + bcd[14] * 0.01 + \
            bcd[13] * 0.001 +  + bcd[12] * 0.0001
        return (mjd - 40587) * 86400 + sec

    def chunk_data(self, src, start, stop, secs_per_chunk, dest, callback, arg):
        num_chunks = int((stop - start) // secs_per_chunk)
        current_chunk = -1
        current_chunk_pos = -1
        pos = 0

        fp = open(src)
        buf = fp.read(self.frame_size + self.sync_size)
        while len(buf) > 0:
            # Search for sync word.
            offset = buf.find('\xed\xde\xad\xab')
            if offset == -1:
                pos += len(buf) - self.sync_size
                buf = buf[-self.sync_size:]
                buf += fp.read(self.frame_size)
                continue

            # Found sync word.  Skip it and make sure we have a full header in
            # the remaining buffer.
            pos += offset + self.sync_size
            buf = buf[offset + self.sync_size:]
            if len(buf) < self.header_size:
                buf += fp.read(self.header_size - len(buf))
                pass

            # If we failed to read a full header, punt.
            if len(buf) < self.header_size:
                break

            secs = self.decode_header(buf)
            if secs >= stop:
                break

            if secs >= start:
                chunk = int((secs - start) // secs_per_chunk)
                if chunk != current_chunk:
                    if current_chunk_pos >= 0:
                        chunk_start = start + current_chunk * secs_per_chunk
                        chunk_stop = chunk_start + secs_per_chunk
                        callback(src, dest, current_chunk, num_chunks,
                                 chunk_start, chunk_stop, current_chunk_pos,
                                 pos - current_chunk_pos, arg)
                        pass
                    current_chunk = chunk
                    current_chunk_pos = pos
                    pass
                pass

            # Skip the frame and read the next sync word and header at the
            # position where we expect it to be.  If for some reason it
            # isn't there this will trigger a new sync word search.
            pos += self.frame_size - self.sync_size
            fp.seek(pos)
            buf = fp.read(self.sync_size + self.header_size)
            continue

        if current_chunk >= 0:
            chunk_start = start + current_chunk * secs_per_chunk
            chunk_stop = chunk_start + secs_per_chunk
            callback(src, dest, current_chunk, num_chunks,
                     chunk_start, chunk_stop, current_chunk_pos,
                     pos - current_chunk_pos, arg)
            pass

        return

    pass

if __name__ == "__main__":
    d = Mark5B()
    d.chunk_data("/scratch/kettenis/n10c1/n10c1_wb_no0001",
                 vex2time("2010y077d22h00m00s"),
                 vex2time("2010y077d22h02m00s"), 60,
                 "", None, None)
