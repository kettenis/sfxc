################################################## 
# TranslationJob_services_types.py 
# generated by ZSI.generate.wsdl2python
##################################################


import ZSI
import ZSI.TCcompound
from ZSI.schema import LocalElementDeclaration, ElementDeclaration, TypeDefinition, GTD, GED

##############################
# targetNamespace
# http://jobinfo.broker.remote.expres.psnc.pl/xsd
##############################

class ns0:
    targetNamespace = "http://jobinfo.broker.remote.expres.psnc.pl/xsd"

    class TranslationJobInfo_Def(ZSI.TCcompound.ComplexType, TypeDefinition):
        schema = "http://jobinfo.broker.remote.expres.psnc.pl/xsd"
        type = (schema, "TranslationJobInfo")
        def __init__(self, pname, ofwhat=(), attributes=None, extend=False, restrict=False, **kw):
            ns = ns0.TranslationJobInfo_Def.schema
            TClist = [ZSI.TC.String(pname=(ns,"dataLocation"), aname="_dataLocation", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded")), ZSI.TC.String(pname=(ns,"endTime"), aname="_endTime", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded")), GTD("http://protocol.remote.expres.psnc.pl/xsd","MessageHeader",lazy=False)(pname=(ns,"header"), aname="_header", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded")), ZSI.TC.String(pname=(ns,"startTime"), aname="_startTime", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded"))]
            self.attribute_typecode_dict = attributes or {}
            if extend: TClist += ofwhat
            if restrict: TClist = ofwhat
            ZSI.TCcompound.ComplexType.__init__(self, None, TClist, pname=pname, inorder=0, **kw)
            class Holder:
                typecode = self
                def __init__(self):
                    # pyclass
                    self._dataLocation = None
                    self._endTime = None
                    self._header = None
                    self._startTime = None
                    return
            Holder.__name__ = "TranslationJobInfo_Holder"
            self.pyclass = Holder

# end class ns0 (tns: http://jobinfo.broker.remote.expres.psnc.pl/xsd)

##############################
# targetNamespace
# http://tn.remote.expres.psnc.pl
##############################

class ns2:
    targetNamespace = "http://tn.remote.expres.psnc.pl"

    class startTranslationJob_Dec(ZSI.TCcompound.ComplexType, ElementDeclaration):
        literal = "startTranslationJob"
        schema = "http://tn.remote.expres.psnc.pl"
        def __init__(self, **kw):
            ns = ns2.startTranslationJob_Dec.schema
            TClist = [GTD("http://jobinfo.broker.remote.expres.psnc.pl/xsd","TranslationJobInfo",lazy=False)(pname=(ns,"translationJobInfo"), aname="_translationJobInfo", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded"))]
            kw["pname"] = ("http://tn.remote.expres.psnc.pl","startTranslationJob")
            kw["aname"] = "_startTranslationJob"
            self.attribute_typecode_dict = {}
            ZSI.TCcompound.ComplexType.__init__(self,None,TClist,inorder=0,**kw)
            class Holder:
                typecode = self
                def __init__(self):
                    # pyclass
                    self._translationJobInfo = None
                    return
            Holder.__name__ = "startTranslationJob_Holder"
            self.pyclass = Holder

# end class ns2 (tns: http://tn.remote.expres.psnc.pl)

##############################
# targetNamespace
# http://protocol.remote.expres.psnc.pl/xsd
##############################

class ns1:
    targetNamespace = "http://protocol.remote.expres.psnc.pl/xsd"

    class MessageHeader_Def(ZSI.TCcompound.ComplexType, TypeDefinition):
        schema = "http://protocol.remote.expres.psnc.pl/xsd"
        type = (schema, "MessageHeader")
        def __init__(self, pname, ofwhat=(), attributes=None, extend=False, restrict=False, **kw):
            ns = ns1.MessageHeader_Def.schema
            TClist = [ZSI.TCnumbers.Ilong(pname=(ns,"chunkCount"), aname="_chunkCount", minOccurs=0, maxOccurs=1, nillable=False, typed=False, encoded=kw.get("encoded")), ZSI.TCnumbers.Ilong(pname=(ns,"chunkId"), aname="_chunkId", minOccurs=0, maxOccurs=1, nillable=False, typed=False, encoded=kw.get("encoded")), ZSI.TCnumbers.Ilong(pname=(ns,"chunkSize"), aname="_chunkSize", minOccurs=0, maxOccurs=1, nillable=False, typed=False, encoded=kw.get("encoded")), ZSI.TC.String(pname=(ns,"experimentName"), aname="_experimentName", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded")), ZSI.TC.String(pname=(ns,"jobId"), aname="_jobId", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded")), ZSI.TC.String(pname=(ns,"senderLocation"), aname="_senderLocation", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded")), ZSI.TC.String(pname=(ns,"telescopeAbbr"), aname="_telescopeAbbr", minOccurs=0, maxOccurs=1, nillable=True, typed=False, encoded=kw.get("encoded"))]
            self.attribute_typecode_dict = attributes or {}
            if extend: TClist += ofwhat
            if restrict: TClist = ofwhat
            ZSI.TCcompound.ComplexType.__init__(self, None, TClist, pname=pname, inorder=0, **kw)
            class Holder:
                typecode = self
                def __init__(self):
                    # pyclass
                    self._chunkCount = None
                    self._chunkId = None
                    self._chunkSize = None
                    self._experimentName = None
                    self._jobId = None
                    self._senderLocation = None
                    self._telescopeAbbr = None
                    return
            Holder.__name__ = "MessageHeader_Holder"
            self.pyclass = Holder

# end class ns1 (tns: http://protocol.remote.expres.psnc.pl/xsd)