# Googlesheets works with shift

    Code
      str(gs_request)
    Output
      List of 3
       $ method: chr "POST"
       $ url   : chr "https://sheets.googleapis.com/v4/spreadsheets/spreadsheet_id:batchUpdate"
       $ body  :List of 1
        ..$ requests:List of 63
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ updateCells:List of 3
        .. .. .. .. ..$ rows  :List of 2
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 7
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue: logi NA
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue: logi NA
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue: logi NA
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "Horse Power"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue: logi NA
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "Weight"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue: logi NA
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 7
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "Cylinder"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "Engine"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "N"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "Mean"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "SD"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "Mean"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "SD"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. ..$ fields: chr "userEnteredValue,userEnteredFormat"
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. .. ..$ endRowIndex     : int 8
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : int 9
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
        .. .. .. .. ..$ rows  :List of 5
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 7
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 6
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 0
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: int 3
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 132
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 37.5
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 2.75
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 0.128
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 7
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 4
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 1
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: int 10
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 81.8
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 21.9
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 2.3
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 0.598
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 7
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 6
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 1
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: int 4
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 115
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 9.18
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 3.39
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 0.116
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 7
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 8
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 0
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: int 14
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 209
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 51
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 4
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 0.759
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. ..$ values:List of 7
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 4
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 0
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: int 1
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 91
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue: logi NA
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ numberValue: num 2.14
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue: logi NA
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. ..$ fields: chr "userEnteredValue,userEnteredFormat"
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 8
        .. .. .. .. .. ..$ endRowIndex     : int 13
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : int 9
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
        .. .. .. .. .. .. ..$ values:List of 1
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "Motor Trend Car Road Tests"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. ..$ fields: chr "userEnteredValue,userEnteredFormat"
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. .. ..$ endRowIndex     : int 5
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : int 9
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
        .. .. .. .. .. .. ..$ values:List of 1
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "A table created with tablespan"
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. ..$ fields: chr "userEnteredValue,userEnteredFormat"
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 5
        .. .. .. .. .. ..$ endRowIndex     : int 6
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : int 9
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
        .. .. .. .. .. .. ..$ values:List of 1
        .. .. .. .. .. .. .. ..$ :List of 1
        .. .. .. .. .. .. .. .. ..$ userEnteredValue:List of 1
        .. .. .. .. .. .. .. .. .. ..$ stringValue: chr "Data from the infamous mtcars data set."
        .. .. .. .. .. .. .. .. ..- attr(*, "class")= chr [1:3] "googlesheets4_schema_CellData" "googlesheets4_schema" "list"
        .. .. .. .. ..$ fields: chr "userEnteredValue,userEnteredFormat"
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 13
        .. .. .. .. .. ..$ endRowIndex     : int 14
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : int 9
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
        .. .. .. .. .. ..$ startRowIndex   : num 8
        .. .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 9
        .. .. .. .. .. ..$ endRowIndex     : num 11
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 8
        .. .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 9
        .. .. .. .. .. ..$ endRowIndex     : num 11
        .. .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 8
        .. .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. .. ..$ startColumnIndex: num 4
        .. .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 8
        .. .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. .. ..$ endColumnIndex  : num 6
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.00"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 9
        .. .. .. .. .. ..$ endRowIndex     : num 11
        .. .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. .. ..$ endColumnIndex  : num 6
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 8
        .. .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. .. ..$ startColumnIndex: num 6
        .. .. .. .. .. ..$ endColumnIndex  : num 7
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.000"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 9
        .. .. .. .. .. ..$ endRowIndex     : num 11
        .. .. .. .. .. ..$ startColumnIndex: num 6
        .. .. .. .. .. ..$ endColumnIndex  : num 7
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 8
        .. .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. .. ..$ endColumnIndex  : num 8
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.0000"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 9
        .. .. .. .. .. ..$ endRowIndex     : num 11
        .. .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. .. ..$ endColumnIndex  : num 8
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 8
        .. .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. .. ..$ startColumnIndex: num 8
        .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.00"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 9
        .. .. .. .. .. ..$ endRowIndex     : num 11
        .. .. .. .. .. ..$ startColumnIndex: num 8
        .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat  :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ numberFormat:List of 2
        .. .. .. .. .. .. .. ..$ type   : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ pattern: chr "#,##0.0"
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,numberFormat)"
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 4
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 7
        .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. ..$ endColumnIndex  : num 6
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
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 7
        .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. ..$ endColumnIndex  : num 8
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
        .. .. .. .. ..$ startRowIndex   : num 7
        .. .. .. .. ..$ endRowIndex     : num 8
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
        .. .. .. .. ..$ startRowIndex   : num 7
        .. .. .. .. ..$ endRowIndex     : num 8
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
        .. .. .. .. ..$ startRowIndex   : num 7
        .. .. .. .. ..$ endRowIndex     : num 8
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
        .. .. ..$ updateBorders:List of 4
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 7
        .. .. .. .. ..$ endRowIndex     : num 8
        .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. ..$ endColumnIndex  : num 6
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
        .. .. .. .. ..$ startRowIndex   : num 7
        .. .. .. .. ..$ endRowIndex     : num 8
        .. .. .. .. ..$ startColumnIndex: num 6
        .. .. .. .. ..$ endColumnIndex  : num 7
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
        .. .. .. .. ..$ startRowIndex   : num 7
        .. .. .. .. ..$ endRowIndex     : num 8
        .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. ..$ endColumnIndex  : num 8
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
        .. .. .. .. ..$ startRowIndex   : num 7
        .. .. .. .. ..$ endRowIndex     : num 8
        .. .. .. .. ..$ startColumnIndex: num 8
        .. .. .. .. ..$ endColumnIndex  : num 9
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
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 7
        .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. ..$ endColumnIndex  : num 7
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
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 7
        .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. ..$ endColumnIndex  : num 9
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
        .. .. ..$ mergeCells:List of 2
        .. .. .. ..$ range    :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. ..$ mergeType: chr "MERGE_ALL"
        .. ..$ :List of 1
        .. .. ..$ mergeCells:List of 2
        .. .. .. ..$ range    :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 5
        .. .. .. .. ..$ endRowIndex     : num 6
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. ..$ mergeType: chr "MERGE_ALL"
        .. ..$ :List of 1
        .. .. ..$ mergeCells:List of 2
        .. .. .. ..$ range    :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 13
        .. .. .. .. ..$ endRowIndex     : num 14
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. ..$ mergeType: chr "MERGE_ALL"
        .. ..$ :List of 1
        .. .. ..$ mergeCells:List of 2
        .. .. .. ..$ range    :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 7
        .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. ..$ endColumnIndex  : num 7
        .. .. .. ..$ mergeType: chr "MERGE_ALL"
        .. ..$ :List of 1
        .. .. ..$ mergeCells:List of 2
        .. .. .. ..$ range    :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 7
        .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. ..$ mergeType: chr "MERGE_ALL"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 4
        .. .. .. .. .. ..$ endRowIndex     : num 5
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat     :List of 4
        .. .. .. .. .. .. .. ..$ bold           : logi TRUE
        .. .. .. .. .. .. .. ..$ italic         : logi TRUE
        .. .. .. .. .. .. .. ..$ fontSize       : num 12
        .. .. .. .. .. .. .. ..$ foregroundColor:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 1
        .. .. .. .. .. .. .. .. ..$ green: num 1
        .. .. .. .. .. .. .. .. ..$ blue : num 1
        .. .. .. .. .. .. ..$ backgroundColor:List of 3
        .. .. .. .. .. .. .. ..$ red  : num 0.596
        .. .. .. .. .. .. .. ..$ green: num 0.204
        .. .. .. .. .. .. .. ..$ blue : num 0.224
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,backgroundColor)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 5
        .. .. .. .. .. ..$ endRowIndex     : num 6
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat     :List of 4
        .. .. .. .. .. .. .. ..$ bold           : logi TRUE
        .. .. .. .. .. .. .. ..$ italic         : logi TRUE
        .. .. .. .. .. .. .. ..$ fontSize       : num 10
        .. .. .. .. .. .. .. ..$ foregroundColor:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 1
        .. .. .. .. .. .. .. .. ..$ green: num 1
        .. .. .. .. .. .. .. .. ..$ blue : num 1
        .. .. .. .. .. .. ..$ backgroundColor:List of 3
        .. .. .. .. .. .. .. ..$ red  : num 0.596
        .. .. .. .. .. .. .. ..$ green: num 0.204
        .. .. .. .. .. .. .. ..$ blue : num 0.224
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,backgroundColor)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. .. ..$ endRowIndex     : num 8
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 2
        .. .. .. .. .. .. ..$ textFormat     :List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi TRUE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 10
        .. .. .. .. .. .. ..$ backgroundColor:List of 3
        .. .. .. .. .. .. .. ..$ red  : num 0.714
        .. .. .. .. .. .. .. ..$ green: num 0.329
        .. .. .. .. .. .. .. ..$ blue : num 0.333
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat,backgroundColor)"
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 4
        .. .. .. ..$ range :List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 8
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 9
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
        .. .. .. .. .. ..$ startRowIndex   : num 13
        .. .. .. .. .. ..$ endRowIndex     : num 14
        .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. ..$ textFormat:List of 3
        .. .. .. .. .. .. .. ..$ bold    : logi FALSE
        .. .. .. .. .. .. .. ..$ italic  : logi FALSE
        .. .. .. .. .. .. .. ..$ fontSize: num 8
        .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 2
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. .. ..$ :List of 1
        .. .. .. ..$ addConditionalFormatRule:List of 2
        .. .. .. .. ..$ rule :List of 2
        .. .. .. .. .. ..$ ranges      :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. .. .. ..$ gradientRule:List of 3
        .. .. .. .. .. .. ..$ minpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.0706
        .. .. .. .. .. .. .. .. ..$ green: num 0.204
        .. .. .. .. .. .. .. .. ..$ blue : num 0.337
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "0"
        .. .. .. .. .. .. ..$ midpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 1
        .. .. .. .. .. .. .. .. ..$ green: num 1
        .. .. .. .. .. .. .. .. ..$ blue : num 1
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "50"
        .. .. .. .. .. .. ..$ maxpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.706
        .. .. .. .. .. .. .. .. ..$ green: num 0.412
        .. .. .. .. .. .. .. .. ..$ blue : num 0.514
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "209.214285714286"
        .. .. .. .. ..$ index: num 0
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 9
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 11
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. .. .. ..$ endColumnIndex  : num 3
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 3
        .. .. .. .. .. .. .. .. ..$ bold           : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic         : logi TRUE
        .. .. .. .. .. .. .. .. ..$ foregroundColor:List of 3
        .. .. .. .. .. .. .. .. .. ..$ red  : num 0.71
        .. .. .. .. .. .. .. .. .. ..$ green: num 0.263
        .. .. .. .. .. .. .. .. .. ..$ blue : num 0.129
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 2
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. .. ..$ :List of 1
        .. .. .. ..$ addConditionalFormatRule:List of 2
        .. .. .. .. ..$ rule :List of 2
        .. .. .. .. .. ..$ ranges      :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. .. .. ..$ gradientRule:List of 3
        .. .. .. .. .. .. ..$ minpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.0706
        .. .. .. .. .. .. .. .. ..$ green: num 0.204
        .. .. .. .. .. .. .. .. ..$ blue : num 0.337
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "0"
        .. .. .. .. .. .. ..$ midpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 1
        .. .. .. .. .. .. .. .. ..$ green: num 1
        .. .. .. .. .. .. .. .. ..$ blue : num 1
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "50"
        .. .. .. .. .. .. ..$ maxpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.706
        .. .. .. .. .. .. .. .. ..$ green: num 0.412
        .. .. .. .. .. .. .. .. ..$ blue : num 0.514
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "209.214285714286"
        .. .. .. .. ..$ index: num 0
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 9
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 11
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 3
        .. .. .. .. .. .. ..$ endColumnIndex  : num 4
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 3
        .. .. .. .. .. .. .. .. ..$ bold           : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic         : logi TRUE
        .. .. .. .. .. .. .. .. ..$ foregroundColor:List of 3
        .. .. .. .. .. .. .. .. .. ..$ red  : num 0.71
        .. .. .. .. .. .. .. .. .. ..$ green: num 0.263
        .. .. .. .. .. .. .. .. .. ..$ blue : num 0.129
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
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
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. .. .. ..$ endColumnIndex  : num 6
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 2
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. .. .. ..$ endColumnIndex  : num 6
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. .. ..$ :List of 1
        .. .. .. ..$ addConditionalFormatRule:List of 2
        .. .. .. .. ..$ rule :List of 2
        .. .. .. .. .. ..$ ranges      :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. .. .. ..$ endColumnIndex  : num 6
        .. .. .. .. .. ..$ gradientRule:List of 3
        .. .. .. .. .. .. ..$ minpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.0706
        .. .. .. .. .. .. .. .. ..$ green: num 0.204
        .. .. .. .. .. .. .. .. ..$ blue : num 0.337
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "0"
        .. .. .. .. .. .. ..$ midpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 1
        .. .. .. .. .. .. .. .. ..$ green: num 1
        .. .. .. .. .. .. .. .. ..$ blue : num 1
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "50"
        .. .. .. .. .. .. ..$ maxpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.706
        .. .. .. .. .. .. .. .. ..$ green: num 0.412
        .. .. .. .. .. .. .. .. ..$ blue : num 0.514
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "209.214285714286"
        .. .. .. .. ..$ index: num 0
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 9
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 11
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 5
        .. .. .. .. .. .. ..$ endColumnIndex  : num 6
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 3
        .. .. .. .. .. .. .. .. ..$ bold           : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic         : logi TRUE
        .. .. .. .. .. .. .. .. ..$ foregroundColor:List of 3
        .. .. .. .. .. .. .. .. .. ..$ red  : num 0.71
        .. .. .. .. .. .. .. .. .. ..$ green: num 0.263
        .. .. .. .. .. .. .. .. .. ..$ blue : num 0.129
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 6
        .. .. .. .. .. .. ..$ endColumnIndex  : num 7
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 2
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 6
        .. .. .. .. .. .. ..$ endColumnIndex  : num 7
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. .. ..$ :List of 1
        .. .. .. ..$ addConditionalFormatRule:List of 2
        .. .. .. .. ..$ rule :List of 2
        .. .. .. .. .. ..$ ranges      :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 6
        .. .. .. .. .. .. ..$ endColumnIndex  : num 7
        .. .. .. .. .. ..$ gradientRule:List of 3
        .. .. .. .. .. .. ..$ minpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.0706
        .. .. .. .. .. .. .. .. ..$ green: num 0.204
        .. .. .. .. .. .. .. .. ..$ blue : num 0.337
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "0"
        .. .. .. .. .. .. ..$ midpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 1
        .. .. .. .. .. .. .. .. ..$ green: num 1
        .. .. .. .. .. .. .. .. ..$ blue : num 1
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "50"
        .. .. .. .. .. .. ..$ maxpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.706
        .. .. .. .. .. .. .. .. ..$ green: num 0.412
        .. .. .. .. .. .. .. .. ..$ blue : num 0.514
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "209.214285714286"
        .. .. .. .. ..$ index: num 0
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 9
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 11
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 6
        .. .. .. .. .. .. ..$ endColumnIndex  : num 7
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 3
        .. .. .. .. .. .. .. .. ..$ bold           : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic         : logi TRUE
        .. .. .. .. .. .. .. .. ..$ foregroundColor:List of 3
        .. .. .. .. .. .. .. .. .. ..$ red  : num 0.71
        .. .. .. .. .. .. .. .. .. ..$ green: num 0.263
        .. .. .. .. .. .. .. .. .. ..$ blue : num 0.129
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. .. .. ..$ endColumnIndex  : num 8
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 2
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. .. .. ..$ endColumnIndex  : num 8
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. .. ..$ :List of 1
        .. .. .. ..$ addConditionalFormatRule:List of 2
        .. .. .. .. ..$ rule :List of 2
        .. .. .. .. .. ..$ ranges      :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. .. .. ..$ endColumnIndex  : num 8
        .. .. .. .. .. ..$ gradientRule:List of 3
        .. .. .. .. .. .. ..$ minpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.0706
        .. .. .. .. .. .. .. .. ..$ green: num 0.204
        .. .. .. .. .. .. .. .. ..$ blue : num 0.337
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "0"
        .. .. .. .. .. .. ..$ midpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 1
        .. .. .. .. .. .. .. .. ..$ green: num 1
        .. .. .. .. .. .. .. .. ..$ blue : num 1
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "50"
        .. .. .. .. .. .. ..$ maxpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.706
        .. .. .. .. .. .. .. .. ..$ green: num 0.412
        .. .. .. .. .. .. .. .. ..$ blue : num 0.514
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "209.214285714286"
        .. .. .. .. ..$ index: num 0
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 9
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 11
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 7
        .. .. .. .. .. .. ..$ endColumnIndex  : num 8
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 3
        .. .. .. .. .. .. .. .. ..$ bold           : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic         : logi TRUE
        .. .. .. .. .. .. .. .. ..$ foregroundColor:List of 3
        .. .. .. .. .. .. .. .. .. ..$ red  : num 0.71
        .. .. .. .. .. .. .. .. .. ..$ green: num 0.263
        .. .. .. .. .. .. .. .. .. ..$ blue : num 0.129
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 8
        .. .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 2
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 8
        .. .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 2
        .. .. .. .. .. .. .. .. ..$ bold  : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic: logi FALSE
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. .. ..$ :List of 1
        .. .. .. ..$ addConditionalFormatRule:List of 2
        .. .. .. .. ..$ rule :List of 2
        .. .. .. .. .. ..$ ranges      :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 8
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 13
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 8
        .. .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. .. ..$ gradientRule:List of 3
        .. .. .. .. .. .. ..$ minpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.0706
        .. .. .. .. .. .. .. .. ..$ green: num 0.204
        .. .. .. .. .. .. .. .. ..$ blue : num 0.337
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "0"
        .. .. .. .. .. .. ..$ midpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 1
        .. .. .. .. .. .. .. .. ..$ green: num 1
        .. .. .. .. .. .. .. .. ..$ blue : num 1
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "50"
        .. .. .. .. .. .. ..$ maxpoint:List of 3
        .. .. .. .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. .. .. .. ..$ red  : num 0.706
        .. .. .. .. .. .. .. .. ..$ green: num 0.412
        .. .. .. .. .. .. .. .. ..$ blue : num 0.514
        .. .. .. .. .. .. .. ..$ type : chr "NUMBER"
        .. .. .. .. .. .. .. ..$ value: chr "209.214285714286"
        .. .. .. .. ..$ index: num 0
        .. ..$ :List of 1
        .. .. ..$ :List of 1
        .. .. .. ..$ :List of 1
        .. .. .. .. ..$ repeatCell:List of 3
        .. .. .. .. .. ..$ range :List of 5
        .. .. .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. .. .. ..$ startRowIndex   : Named num 9
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "min"
        .. .. .. .. .. .. ..$ endRowIndex     : Named num 11
        .. .. .. .. .. .. .. ..- attr(*, "names")= chr "max"
        .. .. .. .. .. .. ..$ startColumnIndex: num 8
        .. .. .. .. .. .. ..$ endColumnIndex  : num 9
        .. .. .. .. .. ..$ cell  :List of 1
        .. .. .. .. .. .. ..$ userEnteredFormat:List of 1
        .. .. .. .. .. .. .. ..$ textFormat:List of 3
        .. .. .. .. .. .. .. .. ..$ bold           : logi FALSE
        .. .. .. .. .. .. .. .. ..$ italic         : logi TRUE
        .. .. .. .. .. .. .. .. ..$ foregroundColor:List of 3
        .. .. .. .. .. .. .. .. .. ..$ red  : num 0.71
        .. .. .. .. .. .. .. .. .. ..$ green: num 0.263
        .. .. .. .. .. .. .. .. .. ..$ blue : num 0.129
        .. .. .. .. .. ..$ fields: chr "userEnteredFormat(textFormat)"
        .. ..$ :List of 1
        .. .. ..$ updateBorders:List of 2
        .. .. .. ..$ range:List of 5
        .. .. .. .. ..$ sheetId         : num 2
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 7
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 9
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
        .. .. .. .. ..$ startRowIndex   : num 12
        .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 9
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
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. ..$ startColumnIndex: num 2
        .. .. .. .. ..$ endColumnIndex  : num 3
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
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. ..$ startColumnIndex: num 8
        .. .. .. .. ..$ endColumnIndex  : num 9
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
        .. .. .. .. ..$ startRowIndex   : num 6
        .. .. .. .. ..$ endRowIndex     : num 13
        .. .. .. .. ..$ startColumnIndex: num 4
        .. .. .. .. ..$ endColumnIndex  : num 5
        .. .. .. ..$ left :List of 3
        .. .. .. .. ..$ style: chr "SOLID"
        .. .. .. .. ..$ width: num 1
        .. .. .. .. ..$ color:List of 3
        .. .. .. .. .. ..$ red  : num 0
        .. .. .. .. .. ..$ green: num 0
        .. .. .. .. .. ..$ blue : num 0

