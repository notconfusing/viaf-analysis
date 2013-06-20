import json
from collections import defaultdict


langsofitemjson = open('langsofclus.json', 'r')
langsofitem = json.load(langsofitemjson)
                                                                                                   
#our main datastructure
intersections = defaultdict(int)
numintersections = defaultdict(int)
langtotals = defaultdict(int)

def makeKey(langlist):
    returnkey = ''
    for lang in alllangs:
        if lang in langlist:
            returnkey += '1'
        else:
            returnkey += '0'
    return returnkey

def numOfLangs(binkey):
    #this is the digitsum
        return sum(map(int, binkey))
        

def reverseKey(langsofitem):
    for item, langlist in langsofitem.iteritems():
        itemKey = makeKey(langlist)
        intersections[itemKey] += 1
    #print intersections          
    #print len(intersections)

def reverseKeyFilterN(langsofitem, n):
    total, ngrams = 0, 0
    for item, langlist in langsofitem.iteritems():
        total += 1
        #update lang totals
        for lang in langlist:
            langtotals[lang] += 1
        #record the ones of lenght n
        if len(langlist) == n:
            ngrams += 1
            langlist.sort()
            langlistkey = reduce(lambda x,y: x+'*'+y,langlist)
            intersections[langlistkey] += 1
    numintersections[n] = len(intersections)
    
def composition(langsofitem):
    for item, langlist in langsofitem.iteritems():
        langtot = len(langlist)
        numintersections[langtot] += 1
        if langtot > 23:
            print item


def exportToJSON(intersections):
    filternjson = open('filtern.json', 'w')
    json.dump(obj=intersections, fp=filternjson)
    #langtotalsjson = open('langtotals.json', 'w')
    #json.dump(obj=langtotals, fp=langtotalsjson)

def exportToR(intersections):
    csvfile = open('langsofitemR.csv', 'w')
    line = ''
    #write headers
    for lang in alllangs:
        if line == '':
            line += lang
        else: 
            line += (', ' + lang)
    line += '\n'
    csvfile.write(line)
    line = ''
    for binaryRepresentation, count in intersections.iteritems():
        for binary in binaryRepresentation:
            line += (binary + ',')
        line += (count + '\n')  
    

    
#reverseKey(langsofitem)for i in range(1, 32)
print 'loading'
composition(langsofitem)
exportToJSON(numintersections)

#reverseKeyFilterN(langsofitem, 3)
#exportToJSON(intersections)
#exportToR(intersections)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
