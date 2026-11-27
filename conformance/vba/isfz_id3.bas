Attribute VB_Name = "ModIsfzid3"
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
    ws.Range("F1").FormatConditions.Add 1, 3, "=1"
    wb.Names.Add "nm", "=Sheet1!$A$1"
    Setup = "ok"
End Function
Public Function Case_001() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = wb.Names("nm")
    x.RefersTo = "=Sheet1!$C$1"
    Set a = x
    Set b = wb.Names("nm")
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
    Set x = wb.Names("nm")
    x.Value = "=Sheet1!$C$2"
    Set a = x
    Set b = wb.Names("nm")
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
    Set x = wb.Names("nm")
    x.Visible = False
    x.Visible = True
    Set a = x
    Set b = wb.Names("nm")
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
    Set x = wb.Names("nm")
    x.Comment = "c"
    Set a = x
    Set b = wb.Names("nm")
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
    Set x = wb.Names("nm")
    ws.Range("C1").Name = "nm"
    Set a = x
    Set b = wb.Names("nm")
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
    Set x = wb.Names("nm")
    ws.Rows(1).Insert
    ws.Rows(1).Delete
    Set a = x
    Set b = wb.Names("nm")
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
    Set x = wb.Names("nm")
    wb.Names.Add "other", "=1"
    Set a = x
    Set b = wb.Names("nm")
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
    Set x = wb.Names("nm")
    x.RefersToR1C1 = "=Sheet1!R1C1"
    Set a = x
    Set b = wb.Names("nm")
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
    Set x = wb.Names("nm")
    x.RefersToLocal = "=Sheet1!$A$1"
    Set a = x
    Set b = wb.Names("nm")
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
    Set c = ws.ListObjects(1)
    Set x = c.ListColumns(1)
    c.ListColumns.Add 1
    Set a = x
    Set b = c.ListColumns(2)
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
    Set c = ws.ListObjects(1)
    Set x = c.ListColumns(2)
    c.ListColumns(1).Delete
    Set a = x
    Set b = c.ListColumns(1)
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
    Set c = ws.ListObjects(1)
    Set x = c.ListColumns.Add
    Set a = x
    Set b = c.ListColumns(x.Index)
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
    Set c = ws.ListObjects(1)
    Set x = c.ListColumns(1)
    x.Name = "k2"
    Set a = x
    Set b = c.ListColumns(1)
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
    Set a = ws.Range("A2").ListObject.ListColumns(1)
    Set b = ws.ListObjects(1).ListColumns(1)
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
    Set a = ws.Range("A2").ListObject.ListColumns(1).Parent
    Set b = ws.ListObjects(1)
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
    Set x = ws.ListObjects(1)
    x.Resize ws.Range("A1:B5")
    Set a = x
    Set b = ws.ListObjects(1)
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
    ws.Range("J1").Value = "z"
    ws.ListObjects.Add(1, ws.Range("J1:J2"), , 1).Name = "T3"
    Set x = ws.ListObjects(1)
    ws.ListObjects("T3").Unlist
    Set a = x
    Set b = ws.ListObjects(1)
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
    Set x = ws.Range("A1")
    Set a = x.Characters.Parent
    Set b = x
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
    Set x = ws.Range("A1")
    Set a = x.FormatConditions.Parent
    Set b = x
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
    Set x = ws.Range("A1")
    Set a = x.Hyperlinks.Parent
    Set b = x
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
    Set x = ws.Range("A1")
    Set a = x.Borders(7).Parent
    Set b = x
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
    Set x = ws.Range("A1")
    Set a = x.DisplayFormat.Parent
    Set b = x
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
    Set x = ws.Range("A1")
    Set a = x.Rows.Parent
    Set b = ws
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
    Set x = ws.Range("A1")
    Set a = x.Cells.Parent
    Set b = ws
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
    Set x = ws.Range("A1")
    Set a = x.Font.Parent
    Set b = x.Interior.Parent
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
    Set c = ws.Cells
    Set a = c.Font.Parent
    Set b = c
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
    Set a = ws.Range("F1").FormatConditions(1).Interior.Parent
    Set b = ws.Range("F1").FormatConditions(1)
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
    Set x = wb.Styles("Normal")
    Set a = x.Font.Parent
    Set b = x
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
    Set a = ws.Shapes("box").Line
    Set b = ws.Shapes("box").Line
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
    Set a = ws.Shapes("box").Shadow
    Set b = ws.Shapes("box").Shadow
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
    Set a = ws.Shapes("box").TextFrame2
    Set b = ws.Shapes("box").TextFrame2
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
    Set a = ws.Shapes("box").Fill.ForeColor
    Set b = ws.Shapes("box").Fill.ForeColor
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
    Set x = ws.Shapes("box")
    x.Name = "box2"
    Set a = x
    Set b = ws.Shapes("box2")
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
    Set x = ws.Shapes("box2")
    x.Name = "box"
    x.Name = "box2"
    Set a = x
    Set b = ws.Shapes("box2")
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
    Set a = ws.Shapes.Parent
    Set b = ws
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
    app.DisplayAlerts = False
    Set x = wb.Worksheets.Add
    x.Name = "tmp"
    x.Delete
    wb.Worksheets.Add.Name = "tmp"
    Set a = x
    Set b = wb.Worksheets("tmp")
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
    Set x = wb.Worksheets("Sheet2")
    x.Copy , wb.Worksheets(wb.Worksheets.Count)
    Set a = x
    Set b = wb.Worksheets("Sheet2")
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
    Set x = ws.Range("A1").Font
    Case_038 = CStr(x.Parent Is x.Parent)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_038 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
