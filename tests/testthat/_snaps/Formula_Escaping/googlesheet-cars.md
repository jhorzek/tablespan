# formula-esaping works

    Code
      str(gs_request)
    Output
      List of 3
       $ method: chr "POST"
       $ url   : chr "https://sheets.googleapis.com/v4/spreadsheets/spreadsheet_id:batchUpdate"
       $ body  :List of 1
        ..$ requests:List of 21
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ updateCells:List of 3
        .. .. .. .. ..$ rows  :List of 1
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 4
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "'=A1"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "'+A1"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "'-A1"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "'@A1"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. ..$ fields: chr "userEnteredValue,userEnteredFormat"
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. .. ..$ endRowIndex     : int 4
        .. .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. .. ..$ endColumnIndex  : int 5
        .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_GridRange" "googlesheets4_schema" "list"
        .. .. .. .. .. ..- attr(*, "schema")= tibble [5 x 6] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. ..$ property   : chr [1:5] "endColumnIndex" "endRowIndex" "sheetId" "startColumnIndex" ...
        .. .. .. .. .. .. ..$ type       : chr [1:5] "integer" "integer" "integer" "integer" ...
        .. .. .. .. .. .. ..$ instance_of: chr [1:5] NA NA NA NA ...
        .. .. .. .. .. .. ..$ array_of   : chr [1:5] NA NA NA NA ...
        .. .. .. .. .. .. ..$ format     : chr [1:5] "int32" "int32" "int32" "int32" ...
        .. .. .. .. .. .. ..$ enum       :List of 5
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. ..- attr(*, "id")= chr "GridRange"
        .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_UpdateCellsRequest" "googlesheets4_schema" "list"
        .. .. .. .. ..- attr(*, "schema")= tibble [4 x 6] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. ..$ property   : chr [1:4] "fields" "range" "rows" "start"
        .. .. .. .. .. ..$ type       : chr [1:4] "string" "object" "array" "object"
        .. .. .. .. .. ..$ instance_of: chr [1:4] NA "GridRange" NA "GridCoordinate"
        .. .. .. .. .. ..$ array_of   : chr [1:4] NA NA "RowData" NA
        .. .. .. .. .. ..$ format     : chr [1:4] "google-fieldmask" NA NA NA
        .. .. .. .. .. ..$ enum       :List of 4
        .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. ..- attr(*, "id")= chr "UpdateCellsRequest"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ updateCells:List of 3
        .. .. .. .. ..$ rows  :List of 1
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 4
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "'=A1"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "'+A1"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "'-A1"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "'@A1"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. ..$ fields: chr "userEnteredValue,userEnteredFormat"
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. .. ..$ endRowIndex     : int 5
        .. .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. .. ..$ endColumnIndex  : int 5
        .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_GridRange" "googlesheets4_schema" "list"
        .. .. .. .. .. ..- attr(*, "schema")= tibble [5 x 6] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. ..$ property   : chr [1:5] "endColumnIndex" "endRowIndex" "sheetId" "startColumnIndex" ...
        .. .. .. .. .. .. ..$ type       : chr [1:5] "integer" "integer" "integer" "integer" ...
        .. .. .. .. .. .. ..$ instance_of: chr [1:5] NA NA NA NA ...
        .. .. .. .. .. .. ..$ array_of   : chr [1:5] NA NA NA NA ...
        .. .. .. .. .. .. ..$ format     : chr [1:5] "int32" "int32" "int32" "int32" ...
        .. .. .. .. .. .. ..$ enum       :List of 5
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. ..- attr(*, "id")= chr "GridRange"
        .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_UpdateCellsRequest" "googlesheets4_schema" "list"
        .. .. .. .. ..- attr(*, "schema")= tibble [4 x 6] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. ..$ property   : chr [1:4] "fields" "range" "rows" "start"
        .. .. .. .. .. ..$ type       : chr [1:4] "string" "object" "array" "object"
        .. .. .. .. .. ..$ instance_of: chr [1:4] NA "GridRange" NA "GridCoordinate"
        .. .. .. .. .. ..$ array_of   : chr [1:4] NA NA "RowData" NA
        .. .. .. .. .. ..$ format     : chr [1:4] "google-fieldmask" NA NA NA
        .. .. .. .. .. ..$ enum       :List of 4
        .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. .. ..$ : tibble [0 x 2] (S3: tbl_df/tbl/data.frame)
        .. .. .. .. .. .. .. ..$ enum    : chr(0) 
        .. .. .. .. .. .. .. ..$ enumDesc: chr(0) 
        .. .. .. .. .. ..- attr(*, "id")= chr "UpdateCellsRequest"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. .. ..$ endColumnIndex  : num 2
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 1
        .. .. .. .. .. .. .. ..$ type: chr "TEXT"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 1
        .. .. .. .. .. .. .. ..$ type: chr "TEXT"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 1
        .. .. .. .. .. .. .. ..$ type: chr "TEXT"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. .. ..$ startColumnIndex: num 4
        .. .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 1
        .. .. .. .. .. .. .. ..$ type: chr "TEXT"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 4
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 4
        .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. ..$ endColumnIndex  : num 2
        .. .. .. ..$ bottom:List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ left  :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ right :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 4
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 4
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. ..$ bottom:List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ left  :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ right :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 4
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 4
        .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. ..$ bottom:List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ left  :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ right :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 4
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 4
        .. .. .. .. ..$ startColumnIndex: num 4
        .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. ..$ bottom:List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ left  :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ right :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. .. ..$ endRowIndex     : num 4
        .. .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. ..$ textFormat:List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 4
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 4
        .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. ..$ bottom:List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ left  :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. .. .. ..$ right :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 4
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 5
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. .. .. ..$ endColumnIndex  : num 2
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 4
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 5
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 4
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 5
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 4
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 5
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 4
        .. .. .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 2
        .. .. .. ..$ range:List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 4
        .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. ..$ top  :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 2
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. ..$ bottom:List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 2
        .. .. .. ..$ range:List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. ..$ startColumnIndex: num 1
        .. .. .. .. ..$ endColumnIndex  : num 2
        .. .. .. ..$ left :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 2
        .. .. .. ..$ range:List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. ..$ startColumnIndex: num 4
        .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. ..$ right:List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 2
        .. .. .. ..$ range:List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 3
        .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. ..$ left :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0

