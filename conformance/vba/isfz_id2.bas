Attribute VB_Name = "ModIsfzid2"
Public Function Setup() As String
    On Error Resume Next
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    ws.Range("A1:B3").Value = 1
    ws.Range("A1").Value = "k"
    ws.Range("B1").Value = "v"
    ws.Range("A2").Value = "p"
    ws.Range("A3").Value = "q"
    ws.ListObjects.Add(1, ws.Range("A1:B3"), , 1).Name = "T1"
    ws.Shapes.AddShape(1, 200, 10, 50, 40).Name = "box"
    ws.ChartObjects.Add(200, 100, 200, 150).Name = "cht"
    ws.ChartObjects("cht").Chart.SetSourceData ws.Range("D1:D3")
    ws.Range("D1").AddComment "note"
    ws.Hyperlinks.Add ws.Range("E1"), "http://example.invalid/"
    ws.Range("F1").FormatConditions.Add 1, 3, "=1"
    ws.Range("G1").Validation.Add 1, 1, 1, "1", "9"
    ws.Buttons.Add(300, 10, 60, 20).Name = "btn"
    wb.Worksheets("Sheet2").Range("A1:B3").Value = 2
    wb.Worksheets("Sheet2").Range("A1").Value = "k"
    wb.Worksheets("Sheet2").Range("B1").Value = "v"
    wb.PivotCaches.Create(1, wb.Worksheets("Sheet2").Range("A1:B3")).CreatePivotTable wb.Worksheets("Sheet2").Range("E5"), "pv"
    wb.Worksheets("Sheet2").Range("H1:I3").Value = 3
    wb.Worksheets("Sheet2").Range("H1:I3").AutoFilter
    Setup = "ok"
End Function
Public Function Case_001() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes(1)
    Set b = ws.Shapes(1)
    Case_001 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_001 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_002() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes("box")
    Set b = ws.Shapes.Item(1)
    Case_002 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_002 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_003() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    For Each c In ws.Shapes
    Exit For
    Next
    Set a = c
    Set b = ws.Shapes("box")
    Case_003 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_003 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_004() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Shapes.AddShape(1, 300, 100, 20, 20)
    Set a = x
    Set b = ws.Shapes(x.Name)
    Case_004 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_004 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_005() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes("box").Parent
    Set b = ws
    Case_005 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_005 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_006() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes.Range("box")
    Set b = ws.Shapes.Range("box")
    Case_006 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_006 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_007() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes.Range("box").Item(1)
    Set b = ws.Shapes("box")
    Case_007 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_007 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_008() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes("box").Fill
    Set b = ws.Shapes("box").Fill
    Case_008 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_008 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_009() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes("box").TextFrame
    Set b = ws.Shapes("box").TextFrame
    Case_009 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_009 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_010() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes("box").TopLeftCell
    Set b = ws.Shapes("box").TopLeftCell
    Case_010 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_010 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_011() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes("btn")
    Set b = ws.Shapes("btn")
    Case_011 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_011 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_012() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes("btn").ControlFormat
    Set b = ws.Shapes("btn").ControlFormat
    Case_012 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_012 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_013() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Buttons("btn")
    Set b = ws.Buttons("btn")
    Case_013 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_013 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_014() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.DrawingObjects("box")
    Set b = ws.DrawingObjects("box")
    Case_014 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_014 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_015() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ChartObjects("cht")
    Set b = ws.ChartObjects("cht")
    Case_015 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_015 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_016() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ChartObjects("cht").Chart
    Set b = ws.ChartObjects("cht").Chart
    Case_016 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_016 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_017() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes("cht")
    Set b = ws.Shapes("cht")
    Case_017 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_017 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_018() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ChartObjects("cht").Chart.SeriesCollection(1)
    Set b = ws.ChartObjects("cht").Chart.SeriesCollection(1)
    Case_018 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_018 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_019() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ChartObjects("cht").Chart.Parent
    Set b = ws.ChartObjects("cht")
    Case_019 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_019 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_020() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ChartObjects("cht").Parent
    Set b = ws
    Case_020 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_020 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_021() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A2").ListObject
    Set a = x
    Set b = ws.Range("A2").ListObject
    Case_021 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_021 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_022() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("A2").ListObject
    Set b = ws.Range("A2").ListObject
    Case_022 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_022 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_023() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1)
    Set b = ws.ListObjects(1)
    Case_023 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_023 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_024() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects("T1")
    Set b = ws.ListObjects(1)
    Case_024 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_024 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_025() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("A2").ListObject
    Set b = ws.ListObjects(1)
    Case_025 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_025 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_026() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    For Each c In ws.ListObjects
    Exit For
    Next
    Set a = c
    Set b = ws.ListObjects(1)
    Case_026 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_026 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_027() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.ListObjects(1)
    x.Name = "T9"
    x.Name = "T1"
    Set a = x
    Set b = wb.Worksheets(1).ListObjects(1)
    Case_027 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_027 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_028() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    ws.Range("J1").Value = "z"
    Set x = ws.ListObjects.Add(1, ws.Range("J1:J2"), , 1)
    x.Name = "T2"
    Set a = x
    Set b = ws.ListObjects("T2")
    Case_028 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_028 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_029() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).Parent
    Set b = ws
    Case_029 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_029 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_030() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).Range
    Set b = ws.ListObjects(1).Range
    Case_030 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_030 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_031() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).ListColumns
    Set b = ws.ListObjects(1).ListColumns
    Case_031 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_031 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_032() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).ListColumns(1)
    Set b = ws.ListObjects(1).ListColumns(1)
    Case_032 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_032 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_033() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).ListColumns(1)
    Set b = ws.ListObjects(1).ListColumns("k")
    Case_033 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_033 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_034() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    For Each c In ws.ListObjects(1).ListColumns
    Exit For
    Next
    Set a = c
    Set b = ws.ListObjects(1).ListColumns(1)
    Case_034 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_034 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_035() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).ListRows(1)
    Set b = ws.ListObjects(1).ListRows(1)
    Case_035 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_035 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_036() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).Sort
    Set b = ws.ListObjects(1).Sort
    Case_036 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_036 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_037() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).AutoFilter
    Set b = ws.ListObjects(1).AutoFilter
    Case_037 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_037 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_038() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).TableStyle
    Set b = ws.ListObjects(1).TableStyle
    Case_038 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_038 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_039() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects(1).ListColumns(1).Parent
    Set b = ws.ListObjects(1)
    Case_039 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_039 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_040() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("D1").Comment
    Set b = ws.Range("D1").Comment
    Case_040 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_040 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_041() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("D1").Comment
    Set b = ws.Comments(1)
    Case_041 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_041 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_042() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("D1").Comment.Shape
    Set b = ws.Range("D1").Comment.Shape
    Case_042 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_042 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_043() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Hyperlinks(1)
    Set b = ws.Hyperlinks(1)
    Case_043 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_043 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_044() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Hyperlinks(1)
    Set b = ws.Range("E1").Hyperlinks(1)
    Case_044 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_044 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_045() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("F1").FormatConditions(1)
    Set b = ws.Range("F1").FormatConditions(1)
    Case_045 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_045 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_046() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("G1").Validation
    Set b = ws.Range("G1").Validation
    Case_046 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_046 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_047() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.AutoFilter
    Set b = c.AutoFilter
    Case_047 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_047 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_048() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.AutoFilter.Filters
    Set b = c.AutoFilter.Filters
    Case_048 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_048 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_049() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.AutoFilter.Filters(1)
    Set b = c.AutoFilter.Filters(1)
    Case_049 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_049 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_050() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.AutoFilter.Range
    Set b = c.AutoFilter.Range
    Case_050 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_050 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_051() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.PivotTables("pv")
    Set b = c.PivotTables("pv")
    Case_051 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_051 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_052() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.PivotTables("pv")
    Set b = c.PivotTables(1)
    Case_052 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_052 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_053() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.Range("E5").PivotTable
    Set b = c.PivotTables("pv")
    Case_053 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_053 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_054() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.PivotCaches(1)
    Set b = wb.PivotCaches(1)
    Case_054 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_054 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_055() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.PivotTables("pv").PivotCache
    Set b = wb.PivotCaches(1)
    Case_055 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_055 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_056() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.PivotTables("pv").PivotFields("k")
    Set b = c.PivotTables("pv").PivotFields("k")
    Case_056 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_056 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_057() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.PivotCaches
    Set b = wb.PivotCaches
    Case_057 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_057 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_058() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = wb.Worksheets("Sheet2")
    Set a = c.PivotTables("pv").Parent
    Set b = c
    Case_058 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_058 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
