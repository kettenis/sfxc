#!/usr/bin/env python2.4
import sys, re, time, os, math, glob
import traceback, datetime, threading
import Numeric as Nu, itertools
import simplejson
from ZSI import dispatch
from TranslationJobZSI.TranslationJob_services_server import *
from Notification.TranslationNodeNotification import *
import mark5 as mk5tools
import TranslationNode_vex as vextools
conf =  simplejson.load(file('/home/small/code/webservices/translationnode/service_conf.js'))
portMark5Data = conf['portMark5Data']
portMark5Control = conf['portMark5Control']
ipMark = conf['ipMark']
host = conf['host']
localPath = conf['localPath']
portNumber = conf['portNumber']
vexFilePath = conf["vexFilePath"]

def pairwise(l):
    for p in itertools.izip(l[:-1], l[1:]):
        yield p

def fixStartTime(chunk_times, scan_data_start):
    for i in range(len(chunk_times)-1):
        if chunk_times[i] < scan_data_start < chunk_times[i+1]:
            break
    new_times = [scan_data_start] + chunk_times[i+1:]
    return new_times            

class TimeToByteConverter(object):
    def __init__(self, time, byte, datarate):
        quant_time, frac_time = math.modf(time)
        quant_byte = byte - frac_time*datarate
        self.time = quant_time
        self.byte = quant_byte
        self.datarate = datarate
    def timeToByte(self, time):
        return int(self.byte + (time - self.time)*self.datarate)

def vexGetDataRate(vex, scan, station):
    # Calculate the data rate for this scan.
    mode = vex.get_mode(scan)
    bits_per_sample = vex.get_bits_per_sample(mode, station)
    num_channels = vex.get_num_channels(mode, station)
    sample_rate = vex.get_sample_rate(mode, station)
    data_format = vex.get_data_format(scan, station)
    overhead = vextools.data_format_overhead[data_format]
    data_byte_rate = num_channels * sample_rate * bits_per_sample * overhead / 8
    print >>sys.stderr, "bits per sample:", bits_per_sample
    print >>sys.stderr, "number of channels:", num_channels
    print >>sys.stderr, "sample rate:", sample_rate
    print >>sys.stderr, "data rate (bytes):", data_byte_rate 
    return data_byte_rate

class Sched(object):
    def __init__(self, sched):
        self.sched = sched
    def getScans(self):
        return list(self.sched.iterkeys())
    def getScanRange(self, scan):
        scan_start = vextools.parse_vex_time(self.sched[scan]['start'])
        # Find out the true length of the scan, which is just the maximum
        # of the per-station length of the scan.
        def extractLength(info):
            return float(info[2].split()[0])
        secs = max([extractLength(info) for info in self.sched[scan].getall('station')])
        scan_end = scan_start + secs
        return scan_start, scan_end
    def isScanRelevant(self, scan, job_start, job_end):
        scan_start, scan_end = self.getScanRange(scan)
        return not (job_start > scan_end  or scan_start > job_end)

def ackJob(p):
    print 'Requested broker IP address:', p.BrokerLocation
    print 'Requested start time:', p.StartTime
    print 'Requested end time:', p.EndTime
    print 'Requested chunk size:', p.ChunkSize
    print 'Requested data location:', p.DataLocation[0]
    print 'Requested telescope name:', p.TelescopeName
    print 'Requested experiment name:', p.ExperimentName
    print 'GridFTP location:', p.GridFTPLocation

def quantizeChunkDT(requested_chunk_size, data_byte_rate):
    # chunk_length should be an *integer*(>0) number of seconds
    chunk_length = max(math.floor(requested_chunk_size/data_byte_rate), 1.0)
    return chunk_length

def dataChunker(station, vex_fn, exptname, job_start, job_end, requested_chunk_size):
    vex = vextools.Vex(vex_fn)
    sched = Sched(vex['SCHED'])
    ## Real mark5:
    ## mark5 = mk5tools.Mark5(portMark5Control, ipMark, experiment_name, station)
    ## Fake mark5
    mark5 = mk5tools.Mark5Emulator(exptname, station, vex)
    globalChunkNumber = 0
    for scan in sched.getScans():
        if not sched.isScanRelevant(scan, job_start, job_end):
            continue
        print >>sys.stderr, "Scan:", scan
        scan_start, scan_end = sched.getScanRange(scan)
        data_byte_rate = vexGetDataRate(vex, scan, station)
        scan_size_bytes = (scan_end-scan_start)*data_byte_rate
        if (requested_chunk_size == 0):
            chunk_dt = quantizeChunkDT(scan_size_bytes, data_byte_rate)
        else:
            chunk_dt = quantizeChunkDT(requested_chunk_size, data_byte_rate)
        print >>sys.stderr, "Scan boundaries:", 
        print >>sys.stderr, vextools.format_vex_time(scan_start), vextools.format_vex_time(scan_end)
        print >>sys.stderr, "Job boundaries:", 
        print >>sys.stderr, vextools.format_vex_time(job_start), vextools.format_vex_time(job_end)
        scan_data_start = vextools.parseFractionalTime(mark5.getScanStart(scan))
        start_time = max(job_start, scan_start)
        end_time = min(job_end, scan_end)

        if scan_data_start > end_time:
            print >>sys.stderr,"scan_data_start > end_time: Skipping", 
            print >>sys.stderr, (vextools.format_vex_time(scan_data_start), 
                                 vextools.format_vex_time(end_time))
            continue
        print >>sys.stderr, "t1, t2, dt", 
        print >>sys.stderr, vextools.format_vex_time(start_time), vextools.format_vex_time(end_time), chunk_dt
        chunk_times = Nu.arange(start_time, end_time, chunk_dt).tolist() + [end_time]
        if scan_data_start > start_time:
            chunk_times = fixStartTime(chunk_times, scan_data_start)
        print >>sys.stderr, "Chunk times:", ", ".join([vextools.format_vex_time(t) 
                                                       for t in chunk_times])
        for i, (chunk_start, chunk_end) in enumerate(pairwise(chunk_times)):
            sendFile = os.path.join(localPath, (exptname + '_' + station + '_' + 
                                                str(scan) + "_" + "%03d" % i + '.m5a').lower())
            chunk_real_size = mark5.getChunksByTime(sendFile, chunk_start, chunk_end)
            yield (sendFile, globalChunkNumber, chunk_real_size, chunk_start, chunk_end) 
            globalChunkNumber += 1

    print >>sys.stderr, "Disconnecting from mark5"
    # mark5.disconnect()

def startTranslationJob(p):
    print >>sys.stderr, datetime.datetime.now(), "Welcome to startTranslationNode"
    print >>sys.stderr, p
    try:
        t = threading.Thread(target=startTranslationJob2, args=(p,))
        # t.setDaemon(True)
        t.start()
        return None
    except Exception, e:
        print >>sys.stderr, datetime.datetime.now(), "Failed to thread"
        traceback.print_exc(file=sys.stderr)
        sys.exit(1)

def startTranslationJob2(p):
    try:
        param = p["param0"]
        station = param["telescopeName"]
        brokerIPAddress = param["brokerLocation"]
        requested_chunk_size = int(param["chunkSize"])
        experiment_name = param["experimentName"]
        vex_file_name = os.path.join(vexFilePath, experiment_name.lower() + ".vix")
        gridFtpBaseUrl = os.path.join(param["gridFtpLocation"], experiment_name.lower())
        job_start = vextools.parse_vex_time(param["startTime"])
        job_end = vextools.parse_vex_time(param["endTime"])
    except Exception, e:
        print >>sys.stderr, datetime.datetime.now(), "Failed to decode"
        traceback.print_exc(file=sys.stderr)
        sys.exit(1)
    print >>sys.stderr, "Decoded request"
    try:
        tnn_loc = TranslationNotificationLocator()
        port = tnn_loc.getTranslationNotificationPortType(brokerIPAddress)
    except Exception, e:
        print >>sys.stderr, "Didn't get TranslationNodeNotifier", e
        traceback.print_exc(file=sys.stderr)
        sys.exit(1)

    print >>sys.stderr, "Got TranslationNodeNotifier"
    dc = dataChunker(station, vex_file_name, experiment_name, 
                     job_start, job_end, requested_chunk_size)
    translation_node_ip = "http://huygens.nfra.nl"
    translation_node_id = 20001
    print >>sys.stderr, "Got dataChunker"
    if not dc:
        print >>sys.stderr, "dataChunker empty"
    try:
        for (sendFile, chunk_id, chunk_real_size, chunk_start, chunk_end) in dc:
            basefn = os.path.basename(sendFile)
            gridURL = os.path.join(gridFtpBaseUrl, basefn)
            gftpCommand = ('/huygens_1/jops/globus/bin/globus-url-copy -p 4 -cd file://%s  %s' %
                           (sendFile, gridURL))
            print >>sys.stderr, "Command:", gftpCommand
            os.system(gftpCommand)
            print >>sys.stderr, "send notification to grid broker..."
            req = makeTranslationNotification(chunk_id, gridURL,
                                              chunk_real_size, chunk_start, chunk_end,
                                              translation_node_ip, translation_node_id)
            try:
                port.chunkIsReady(req)
            except httplib.ResponseNotReady:
                print >>sys.stdout, "Axis server returned 202; proceeding"
            except Exception, e:
                print >>sys.stderr, "Failed to send notification to Axis:"
                traceback.print_exc(file=sys.stderr)
    except Exception, e:
        traceback.print_exc(file=sys.stderr)

    print >>sys.stderr, "sent notification to grid broker..."

class Service(TranslationJob):
    def soap_startTranslationJob(self, ps):
	rsp = TranslationJob.soap_startTranslationJob(self, ps)
        print ps
	param = self.request.Param0
        processRequest(param)


if __name__ == "__main__" :
    # dispatch.AsServer(portNumber, (Service('translationnode'),))
    param = simplejson.load(file("trialTranslationRequest.js"))["param0"]
    station = param["telescopeName"]
    brokerIPAddress = param["brokerLocation"]
    requested_chunk_size = int(param["chunkSize"])
    experiment_name = param["experimentName"]
    vex_file_name = os.path.join(vexFilePath, experiment_name.lower() + ".vix")
    gridFtpBaseUrl = os.path.join(param["gridFtpLocation"], experiment_name.lower())
    station = param["telescopeName"]
    requested_chunk_size = param["chunkSize"]
    job_start = vextools.parse_vex_time(param["startTime"])
    job_end = vextools.parse_vex_time(param["endTime"])
    if job_end <= job_start:
        raise RuntimeError, "endtime <= starttime"
    for it in dataChunker(station, vex_file_name, 
                          experiment_name, job_start, job_end, 
                          int(requested_chunk_size)):
        print it
    print "Finished"
