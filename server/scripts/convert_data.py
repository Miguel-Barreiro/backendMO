#!/usr/bin/env python
# -*- coding: utf-8 -*-

#########################################################################
#
# Author: Pedro Nande (pedro.nande@miniclip.com)
# Changed for the DinoPets By: Rui Campos (rui.campos@miniclip.com) 
#
# Copyright 2013, Miniclip SA
#
#########################################################################

"""@package Dino Pets
"""

import os
import sys

from configobj import ConfigObj, Section
import plistlib
from gdocsSpreadsheetReader import *
import atom

atom.MEMBER_STRING_ENCODING = unicode
from copy import copy, deepcopy
import getpass
import pprint
import argparse
import json

from cStringIO import StringIO
import sys


STRING_TYPE             = 'String'
BOOLEAN_TYPE            = 'Boolean'
NUMBER_TYPE             = 'Number'
IMPLICIT_TYPE           = 'Implicit'

BOOLSTRINGS         = ['true', '1', 't', 'y', 'yes', 'yeah', 'yup', 'certainly', 'uh-huh']


def isWhole(x):
    if(x%1 == 0):
        return True
    else:
        return False
        
def isNumber(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def convertStringToType(val, typeString):
    if typeString == STRING_TYPE :
        if val == None:
            return ""
        if type(val) == str or type(val) == unicode:
            return val
        else:
            
            raise Exception( "Val "+ val + "is not a string, type " + str(type(val)) ) 
    elif typeString == BOOLEAN_TYPE :
        if val == None:
            return False
        else:
            if type(val) == str or type(val) == unicode:
                if val.lower() in BOOLSTRINGS:
                    return True
                else:
                    return False
            else:
                raise Exception( "Val "+ val + "is not a string, type " + str(type(val)) )
    elif typeString == NUMBER_TYPE :
        if val == None:
            return 0
        else:
            if type(val) == str or type(val) == unicode:
                if val.strip() == "":
                    return 0
                    
                if isNumber(val):
                    if isWhole(float(val)):
                        return int(val)
                    else:
                        return float(val)
                else:
                    raise Exception("Val "+ val + "is not a number, type " + str(type(val)))
            else:
                raise Exception( "Val "+ val + "is not a string, type " + str(type(val)))
                    
    elif typeString == IMPLICIT_TYPE :
        
        if type(val) == str or type(val) == unicode:
            if isNumber(val):
                if isWhole(float(val)):
                    return int(val)
                else:
                    return float(val)
            else:
                return val
        else:
            raise Exception( "Val "+ val + "is not a string, type " + str(type(val)))
            
    else:
        raise Exception("Invalid field type:-" + typeString + "-")
        
        
            
  


class Converter(object):

    def __init__(self, spreadsheet, config, mode):
        
        self.config = config
        self.spreadsheet = spreadsheet
        self.currentRow = []
        self.currentCol = []
        
        self.currentRowSpan = []
        self.currentColSpan = []
        self.currentDictCol = -1
        self.currentDictRow = -1
        self.currentWorksheet = []
        self.currentType = []
        self.mode = mode
        
        
    
    def process(self, category, section):
        
        #sheet_name  = section['sheet_name']
        map         = section['map']
        output_file = section['output_file']
        
        
        #print "Processing sheet '%s'" % sheet_name
        
        #worksheet = self.spreadsheet.getWorksheet(sheet_name)
        
        
        root = deepcopy(map)
        
        
        root = self.parseConfig(root)
        
        
        if not os.path.exists(self.config['config']['out_dir']):

            os.mkdir(self.config['config']['out_dir'])
            
        file = open(output_file, 'w')
        
        pprint.pprint(root)

        if self.mode == "plist":
        
            plistlib.writePlist(root, file)

        else:
            json_contents = json.dumps(root, sort_keys=True,
                                    indent=4, separators=(',', ': '))
            file.write(json_contents)
        
        file.close()
        
    def parseType(self, typeString):
        if typeString.startswith("type:"):
            typeCol = typeString[5:]
            self.currentTypeCol = int(typeCol)
        else:
            self.currentType = typeString
            
    def parseConfigStringInTuple(self, configMap):
        
        string = configMap[0]
        configMap = configMap[1:]
        print string
        postSplit = string.split("=")
        key = postSplit[0]
        if len(postSplit) != 1:
            value = postSplit[1]
        result = []
        if key == "sheet":
            self.currentWorksheet.append(self.spreadsheet.getWorksheet(value))
            result = self.parseConfigAsTuple(configMap)
            self.currentWorksheet.pop()
            
        elif key == "rowSpan":
            
            span = value.split("<->")
            if span[1] == '':
                span_ = (int(span[0]), self.currentWorksheet[-1].maxRow)
            elif int(span[1]) <= -1:
                span_ = (int(span[0]), self.currentWorksheet[-1].maxRow + int(span[1]) + 1)
            else:
                span_ = (int(span[0]), int(span[1]))
                
            if self.currentDictCol != -1:
                keyCol = self.currentDictCol
                self.currentDictCol = -1
                obj = {}
                for i in range(span_[0], span_[1] + 1):
                    self.currentRow.append(i)
                    
                    key = self.currentWorksheet[-1].getCellValue(self.currentRow[-1], keyCol)

                    val = self.parseConfigAsTuple(configMap)
                    if val != None:
                        obj[key] = val
                    self.currentRow.pop()
                return obj
                
            else:
                
                obj = []
            
                for i in range(span_[0], span_[1] + 1):
                    self.currentRow.append(i)
                    val = self.parseConfigAsTuple(configMap)
                    if val != None:
                        obj.append(val)
                    self.currentRow.pop()
                
                return obj

        elif key == "colSpan":
            
            span = value.split("<->")
            if span[1] == '':
                span_ = (int(span[0]), self.currentWorksheet[-1].maxCol)
            elif int(span[1]) <= -1:
                span_ = (int(span[0]), self.currentWorksheet[-1].maxCol + int(span[1]) + 1)
            else:
                span_ = (int(span[0]), int(span[1]))
                
            if self.currentDictRow != -1:
                keyRow = self.currentDictRow
                self.currentDictRow = -1
                obj = {}
                for i in range(span_[0], span_[1] + 1):
                    self.currentCol.append(i)
                    
                    key = self.currentWorksheet[-1].getCellValue(keyRow, self.currentCol[-1])

                    val = self.parseConfigAsTuple(configMap)
                    if val != None:
                        obj[key] = val
                    self.currentCol.pop()
                return obj
                
            else:
                
                obj = []
            
                for i in range(span_[0], span_[1] + 1):
                    self.currentCol.append(i)
                    val = self.parseConfigAsTuple(configMap)
                    if val != None:
                        obj.append(val)
                    self.currentCol.pop()
                
                return obj

        elif key == "ignoreEmpty":
            self.currentType
            if self.readValue() == "" or self.readValue() == None:
                result = None
            else:
                result = self.parseConfigAsTuple(configMap)


        elif key == "colNumber":
            result = self.currentCol[-1]


        elif key == "rowNumber":
            result = self.currentRow[-1]

        elif key == "sheetName":
            result = self.currentWorksheet[-1].title

        elif key == "allSheets":
            result = []
            for w in self.spreadsheet.worksheets:
                self.currentWorksheet.append(self.spreadsheet.getWorksheet(w))
                result.append(self.parseConfigAsTuple(configMap))
                self.currentWorksheet.pop()
        
        elif key == "spanAsDictWithKeyRow":
        
            self.currentDictRow = int(value)
            result = self.parseConfigAsTuple(configMap)
            self.currentDictRow = -1
                       
        elif key == "spanAsDictWithKeyCol":
            
            
            self.currentDictCol = int(value)
            result = self.parseConfigAsTuple(configMap)
            self.currentDictCol = -1
            

        elif key == "mergeArrays":
            array = []
            obj = self.parseConfigAsTuple(configMap)
            for a in obj:
                array.extend(a)
            result = array

        elif key == "mergeDicts":
            dictionary = {}
            obj = self.parseConfigAsTuple(configMap)
            for a in obj:
                dictionary.update(a)
            result = dictionary

        elif key == "mergeDictsByKey":
            dictionary = {}
            obj = self.parseConfigAsTuple(configMap)
            for dictInArray in obj:
                for keyOfDictInArray in dictInArray.keys():
                    if keyOfDictInArray in dictionary:
                        dictionary[keyOfDictInArray].update(dictInArray[keyOfDictInArray])
                    else:
                        dictionary[keyOfDictInArray] = dictInArray[keyOfDictInArray]
            result = dictionary

        elif key == "row":
            self.currentRow.append(int(value))
            result = self.parseConfigAsTuple(configMap)
            self.currentRow.pop()
        elif key == "col":
            self.currentCol.append(int(value))
            result = self.parseConfigAsTuple(configMap)
            self.currentCol.pop()
        elif key == "typeCol":
            self.currentType.append("col=" + value)
            result = self.parseConfigAsTuple(configMap)
            self.currentType.pop()
        elif key == "typeRow":
            self.currentType.append("row=" + value)
            result = self.parseConfigAsTuple(configMap)
            self.currentType.pop()
        elif key == "type":
            self.currentType.append(value)
            result = self.parseConfigAsTuple(configMap)
            self.currentType.pop()
        else :
            raise Exception("Invalid config")
            
        return result
            
    
    def parseConfig(self, configMap):
        if type(configMap) == tuple:
            return self.parseConfigAsTuple(configMap)
        
        elif type(configMap) == str:
            return self.parseConfigAsTuple([configMap])
            
        elif type(configMap) == dict:
            return self.parseConfigAsDict(configMap)
            
        elif type(configMap) == list:
            return self.parseConfigAsList(configMap)
            
    def parseConfigAsList(self, configMap):
        print configMap
        obj = []
        
        for elem in configMap:
        
            if type(elem) == tuple:
                obj.append(self.parseConfigAsTuple(elem))
            elif type(elem) == str:
                obj.append(self.parseConfigAsTuple([elem]))
            elif type(elem) == dict:
                obj.append(self.parseConfigAsDict(elem))
            elif type(elem) == list:
                obj.append(self.parseConfigAsList(elem))
            
        return obj
        
    
    def parseConfigAsDict(self, configMap):
        obj = {}
        for key, elem in configMap.iteritems():
            print key, elem
            _key = key
            if type(elem) == tuple:
                obj[_key] = self.parseConfigAsTuple(elem)
            elif type(elem) == str:
                obj[_key] = self.parseConfigAsTuple([elem])
            elif type(elem) == dict:
                obj[_key] = self.parseConfigAsDict(elem)
            elif type(elem) == list:
                obj[_key] = self.parseConfigAsList(elem)
               
        return obj
       
    def convertToCurrentType(self, value):

        type_ = self.currentType[-1]
        if type(type_) == int:
            type_ = self.currentWorksheet[-1].getCellValue(self.currentRow[-1], type_)
        elif type_.startswith("col="):
            print "type before " + type_
            type_ = type_.replace('col=','')
            print "type after " + type_
            type_ = self.currentWorksheet[-1].getCellValue(self.currentRow[-1], int(type_))
            print "type final" + type_
        elif type_.startswith("row="):
            type_ = type_.replace('row=','')
            type_ = self.currentWorksheet[-1].getCellValue(int(type_), self.currentCol[-1])

        return convertStringToType(value, type_)
    
    def readValue(self):
            
        return self.currentWorksheet[-1].getCellValue(self.currentRow[-1], self.currentCol[-1])
        
            
        
    def parseConfigAsTuple(self, configMap):
        print configMap
        if len(configMap) == 0:
            return self.convertToCurrentType(self.readValue())
                
        if type(configMap[0]) == str:
            return self.parseConfigStringInTuple(configMap)
            
        elif type(configMap[0]) == dict:
            return self.parseConfigAsDict(configMap[0])
            
        elif type(configMap[0]) == list:
        
            return self.parseConfigAsList(configMap[0])
        else:
            raise Exception("invalid element in tuple")
            
            
            
    def convert(self):
        
        config = self.config['config']
        
        
        for category, section in config['categories'].iteritems():
            
            if isinstance(section, Section):
                
                self.process(category, section)

        

                
                
        
            
def main(args):
    
    parse = argparse.ArgumentParser(prog="convert_data.py")
    parse.add_argument("-s", "--silent",
                       help="prints only when there is an error",
                       action="store_true");
    parse.add_argument("-k", "--docskey",
                     help="the spreadsheet key, overrides the .cfg definition");
    parse.add_argument("-m", "--mode", choices=["json", "plist"], default="plist",
                     help="mode is json or plist (defaults to plist)");
    parse.add_argument("-u", "--docsusername",
                     help="the google docs username");
    parse.add_argument("-p", "--docspassword",
                       help="the google docs password");
    parse.add_argument("config_file", nargs="?",
                       help="the config file");
    parsed_args =  parse.parse_args(args[1:])
    
    config_file = ""
    if parsed_args.config_file:
        
        config_file = parsed_args.config_file
        
    else:
        
        config_file = './config.cfg'


        
    if not os.path.isfile(config_file):
            
        print "Configuration file '%s' not found" % (config_file)
            
        quit()
                
    
    config = ConfigObj(config_file, unrepr=True, interpolation='Template')

    username = ""
    password = ""
    if parsed_args.docsusername:
        username = parsed_args.docsusername
    else:
        username = raw_input('Google Docs Username: ')

    if parsed_args.docspassword:
        password = parsed_args.docspassword
    else:
        password = getpass.getpass()


    # See: http://stackoverflow.com/questions/3287651/download-a-spreadsheet-from-google-docs-using-python
        
        

    if parsed_args.docskey:
        key = parsed_args.docskey
    else:
        if 'gdocsSpreadsheetKey' not in config['config']:
            print "Google Docs spreadsheet key is missing from the config file or parameter"
            quit()
        key = config['config']['gdocsSpreadsheetKey']

    mode = parsed_args.mode
    
    mystdout = StringIO()
    old_stdout = sys.stdout
    if parsed_args.silent:
        sys.stdout = mystdout
        

    try:
        print "Searching document with key: ", key
        spreadsheet = Spreadsheet(username, password, key)
        converter = Converter(spreadsheet, config, mode)

        converter.convert()
    except:

        if parsed_args.silent:
            sys.stdout = old_stdout
            print "ERROR:"
            print "printing program output:"
            print mystdout.getvalue()
        print "Unexpected error:", sys.exc_info()[0]
        raise



if __name__ == '__main__':

    main(sys.argv)
