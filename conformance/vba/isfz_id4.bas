Attribute VB_Name = "ModIsfzid4"
Public Function Setup() As String
    On Error Resume Next
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    ws.ListObjects.Add(1, ws.Range("A1:C3"), , 1).Name = "T1"
    ws.Shapes.AddShape(1, 200, 10, 50, 40).Name = "A"
    ws.Buttons.Add(10, 120, 50, 20).Name = "B1"
    ws.Shapes.AddShape(1, 300, 10, 50, 40).Name = "C"
    Setup = "ok"
End Function
Public Function Case_001() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_001 = CStr(s)
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
    ws.Shapes("A").ZOrder 2
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_002 = CStr(s)
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
    Case_003 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_004 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("A").ZOrder 2
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_005 = CStr(s)
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
    Case_006 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_007 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("A").ZOrder 2
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_008 = CStr(s)
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
    Case_009 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_010 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("A").ZOrder 3
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_011 = CStr(s)
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
    Case_012 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_013 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("A").ZOrder 0
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_014 = CStr(s)
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
    Case_015 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_016 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("A").ZOrder 1
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_017 = CStr(s)
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
    Case_018 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_019 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("C").ZOrder 0
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_020 = CStr(s)
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
    Case_021 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_022 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("C").ZOrder 3
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_023 = CStr(s)
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
    Case_024 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_025 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("C").ZOrder 3
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_026 = CStr(s)
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
    Case_027 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_028 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("C").ZOrder 3
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_029 = CStr(s)
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
    Case_030 = CStr(ws.Shapes("A").ZOrderPosition)
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
    Case_031 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Buttons.Add(10, 200, 50, 20).Name = "B2"
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_032 = CStr(s)
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
    Case_033 = CStr(ws.Shapes("C").ZOrderPosition)
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
    ws.Shapes("C").ZOrder 1
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_034 = CStr(s)
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
    ws.Shapes("A").ZOrder 0
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_035 = CStr(s)
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
    ws.Shapes.AddShape(9, 200, 80, 40, 30).Name = "D"
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_036 = CStr(s)
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
    ws.Shapes("D").ZOrder 3
    ws.Shapes("D").ZOrder 3
    Dim s As String
    For Each x In ws.Shapes: s = s & x.Name & ",": Next
    Case_037 = CStr(s)
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
    Case_038 = CStr(TypeName(ws.DrawingObjects("A")))
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
    Case_039 = CStr(TypeName(ws.DrawingObjects("B1")))
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
    Case_040 = CStr(TypeName(ws.DrawingObjects("D")))
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
    Case_041 = CStr(ws.DrawingObjects("A").Name)
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
    Case_042 = CStr(ws.DrawingObjects("nope").Name)
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
    Set x = ws.ListObjects(1).ListColumns(2)
    x.Delete
    Case_043 = CStr(x.Name)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_043 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
