import xml.etree.ElementTree as ET
from collections import defaultdict
import json
import re
f = file('/data/users/kleinm/viaf/marc.xml', 'r')
save = file('/data/users/kleinm/viaf/langsofclus.json','w+')
#ET.register_namespace(prefix='',uri='http://www.loc.gov/MARC21/slim')

langsofclus = defaultdict(list)


def parseARec(xmlstring):
    global iccus
    viafid = ''
    hasICCU, hasWKP = False, False
    root = ET.fromstring(xmlstring)
    for child in root:
        if child.tag == '{http://www.loc.gov/MARC21/slim}controlfield':
            if child.attrib['tag'] == '001':
                viafid = child.text
                viafid = viafid[4:]
        if child.tag == '{http://www.loc.gov/MARC21/slim}datafield':
            datafield = child
            datafield_attrib = child.attrib
            if datafield_attrib['tag'] == '700':
                for subfield in datafield:
                    if subfield.attrib['code'] == '0':
                        idstring = subfield.text
                        acre = re.compile(r'\((.*?)\)')
                        idmatch = acre.match(idstring)
                        idname = idmatch.group(1)
                        langsofclus[viafid].append(idname)



def parse():
    total = 0
    s = f.readline()
    while s:
        total += 1
        if total % 20000 == 0:
            print total
        parseARec(s)
        #print langsofclus
        s = f.readline()
    json.dump(langsofclus, save, indent=4)
    save.close()

def count():
    total = 0
    s = f.readline()
    while s:
        total += 1
        print total

parse()






'''
for child in root:
    if child.tag == '{http://www.loc.gov/MARC21/slim}datafield':
        datafield = child
        datafield_attrib = child.attrib
        if datafield_attrib['tag'] == '375':
            for subfield in datafield:
                if subfield.attrib['code'] == 'a':
                    if subfield.text == 'unknown':
                        pass
                    else:
                        print subfield.text
'''
