#!/usr/bin/env python
# -*- coding: utf-8 -*-

#########################################################################
#
# Author: Rui Campos (rui.campos@miniclip.com)
#
# Copyright 2013, Miniclip SA
#
#########################################################################

"""@package Dino Pets
"""

import gdata.spreadsheet.service


class Cell:


    def __init__(self, feedData):
    
        self.feedData = feedData
        
        self._parse()
        
        
    def _parse(self):
    
        self.value = self.feedData.cell.text
        if self.value is None:
            self.value = ""
            
        self.formula = self.feedData.cell.inputValue
        self.row = int(self.feedData.cell.row) 
        self.col = int(self.feedData.cell.col)
        
        
    def __repr__(self):
    
        return 'row:%s col:%s value:%s formula:%s\n' % (self.row, self.col, self.value, self.formula)
        
class Worksheet:


    def __init__(self, title, spreadsheetKey, worksheetKey, feedData):
    
        self.spreadsheetKey = spreadsheetKey
        self.worksheetKey = worksheetKey
        self.feedData = feedData
        self.cells = {}
        self.title = title
        
        self._parse()
        
        
    def __repr__(self):
    
        return "###\nsheet: %s \n###\n %s" % (self.title, self.cells)
        
        
    def getCell(self, row, col):
    
        if (row, col) in self.cells.keys():
            return self.cells[(row, col)]
            
        return None
    
    def getCellValue(self, row, col):
    
        if (row, col) in self.cells.keys():
            return self.cells[(row, col)].value
            
        return ""
        
    def getRow(self, row):
    
        if row > self.maxRow:
            return None
            
        orderedArray = []
        keys = self.cells.keys()
        
        for i in range(0, self.maxCol):
        
            if (row, i) in keys:
                orderedArray.append(self.cells[(row, i)])
            else:
                orderedArray.append(None)
        
        return orderedArray
    
    
    def getCol(self, col):
    
        if col > self.maxCol:
            return None
            
        orderedArray = []
        keys = self.cells.keys()
        for i in range(0, self.maxRow):
        
            if (i, col) in keys:
                orderedArray.append(self.cells[(i, col)])
            else:
                orderedArray.append(None)
                
        return orderedArray
        
        
    def _parse(self):
    
        self.maxRow = 0
        self.maxCol = 0
        
        for i, entry in enumerate(self.feedData.entry):
        
            if isinstance(self.feedData, gdata.spreadsheet.SpreadsheetsCellsFeed):
            
                cell = Cell(entry)
                
                if cell.row > self.maxRow:
                    self.maxRow = cell.row 
                if cell.col > self.maxCol:
                    self.maxCol = cell.col 
                    
                self.cells[(cell.row, cell.col)] = cell
                
                
class Spreadsheet:

    def __init__(self, gdocsUsername, gdocsPassword, spreadsheetKey):
        
        ###LOGIN GOOGLE
        gd_client = gdata.spreadsheet.service.SpreadsheetsService()
        gd_client.email = gdocsUsername
        gd_client.password = gdocsPassword
        gd_client.ssl = True
        gd_client.source = "Dino Pets Google Docs Downloader"
        gd_client.ProgrammaticLogin()
        
        ###GET THE WORKSHEETS FOR THE SPREADSHEET
        feed = gd_client.GetWorksheetsFeed(spreadsheetKey)
        
        self.worksheets = {}
        
        for i, entry in enumerate(feed.entry):
            
            id_parts = feed.entry[i].id.text.split('/')
            curr_wksht_id = id_parts[len(id_parts) - 1] #the last item in the url is the worksheet id
            
            ###GET THE CELLS FOR THE WORKSHEET
            list_feed = gd_client.GetCellsFeed(spreadsheetKey, curr_wksht_id)
            
            ###CREATE THE WORKSHEET OBJECT BY PARSING THE CELLS
            self.worksheets[entry.title.text] = Worksheet(entry.title.text, spreadsheetKey, curr_wksht_id, list_feed)
            print "####\n" + entry.title.text + "\n####"
            
    def getWorksheet(self, name):
        return self.worksheets[name]
        

if __name__ == '__main__':

    
    spreadsheet = Spreadsheet("##WRITE MAIL HERE FOR TESTING##", "##WRITE PASS HERE FOR TESTING##", "##WRITE SPREADSHEET KEY HERE FOR TESTING##") 

        