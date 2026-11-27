Attribute VB_Name = "ModIsfzid1"
Public Function Setup() As String
    On Error Resume Next
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    wb.Names.Add "nm", "=Sheet1!$A$1"
    ws.Names.Add "loc", "=Sheet1!$B$1"
    ws.Range("A1").Value = 1
    ws.Activate
    ws.Range("A1").Select
    Setup = "ok"
End Function
Public Function Case_001() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = app
    Set b = app
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
    Set a = app
    Set b = app.Application
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
    Set a = app
    Set b = app.Parent
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
    Set a = ws.Range("A1").Application
    Set b = app
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
    Set a = wb.Parent
    Set b = app
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
    Set a = ThisWorkbook
    Set b = ThisWorkbook
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
    Set a = ThisWorkbook
    Set b = ActiveWorkbook
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
    Set a = app.Workbooks(wb.Name)
    Set b = wb
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
    Set a = ws.Parent
    Set b = wb
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
    Set a = ws.Range("A1").Parent.Parent
    Set b = wb
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
    Set x = app.Workbooks.Add
    Set a = x
    Set b = app.Workbooks(x.Name)
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
    Set x = app.Workbooks.Add
    Set a = x
    Set b = ActiveWorkbook
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
    Set x = app.Workbooks.Add
    Set a = x
    Set b = wb
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
    Set a = app.Workbooks
    Set b = app.Workbooks
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
    Set a = wb.Worksheets
    Set b = wb.Worksheets
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
    Set a = wb.Sheets
    Set b = wb.Sheets
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
    Set a = wb.Worksheets
    Set b = wb.Sheets
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
    Set a = wb.Charts
    Set b = wb.Charts
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
    Set a = wb.Names
    Set b = wb.Names
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
    Set a = ws.Names
    Set b = ws.Names
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
    Set a = app.Names
    Set b = app.Names
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
    Set a = app.Windows
    Set b = app.Windows
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
    Set a = wb.Windows
    Set b = wb.Windows
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
    Set a = app.ActiveWindow
    Set b = app.ActiveWindow
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
    Set a = app.Windows(1)
    Set b = app.Windows(1)
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
    Set a = ActiveWindow
    Set b = wb.Windows(1)
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
    Set a = app.ActiveWindow.Parent
    Set b = wb
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
    Set a = app.ActiveWindow.ActiveSheet
    Set b = app.ActiveSheet
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
    Set a = app.AddIns
    Set b = app.AddIns
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
    Set a = wb.Styles
    Set b = wb.Styles
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
    Set a = wb.Styles("Normal")
    Set b = wb.Styles("Normal")
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
    Set a = app.WorksheetFunction
    Set b = app.WorksheetFunction
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
    Set a = app.AutoCorrect
    Set b = app.AutoCorrect
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
    Set a = wb.Worksheets(1)
    Set b = wb.Worksheets(1)
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
    Set a = wb.Worksheets("Sheet1")
    Set b = wb.Worksheets(1)
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
    Set a = wb.Sheets(1)
    Set b = wb.Worksheets(1)
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
    Set a = wb.Worksheets.Item(1)
    Set b = wb.Sheets.Item("sheet1")
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
    Set a = wb.Worksheets(1)
    Set b = wb.Worksheets(2)
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
    Set a = ActiveSheet
    Set b = wb.Worksheets(ActiveSheet.Name)
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
    Set a = Sheet1
    Set b = ws
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
    Set a = Sheet1
    Set b = Sheet1
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
    Set a = ws.Range("A1").Worksheet
    Set b = ws
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
    Set a = ws.Range("A1").Parent
    Set b = ws
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
    Set a = ws.Next
    Set b = wb.Worksheets("Sheet2")
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
    Set a = ws.Cells(1, 1).Offset(1, 1).Parent
    Set b = ws
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
    For Each c In wb.Worksheets
    Exit For
    Next
    Set a = c
    Set b = wb.Worksheets(1)
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
    Set x = wb.Worksheets("Sheet2")
    x.Name = "Renamed"
    x.Name = "Sheet2"
    Set a = x
    Set b = wb.Worksheets("Sheet2")
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
    Set x = wb.Worksheets("Sheet2")
    x.Move wb.Worksheets(1)
    x.Move , wb.Worksheets(2)
    Set a = x
    Set b = wb.Worksheets("Sheet2")
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
    Set x = wb.Worksheets.Add
    Set a = x
    Set b = wb.Worksheets(x.Name)
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
    Set x = wb.Worksheets.Add
    Set a = x
    Set b = ActiveSheet
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
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    Set a = x.Worksheets(1)
    Set b = x.Worksheets(1)
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
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    Set a = x.Worksheets(1)
    Set b = x.Worksheets(1).Range("A1").Parent
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
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    Set a = x.Worksheets(1)
    Set b = wb.Worksheets(1)
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
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    Set a = x.Worksheets(1).Parent
    Set b = x
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
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    x.Charts.Add
    Set a = x.Charts(1)
    Set b = x.Charts(1)
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
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    x.Charts.Add
    Set a = x.Charts(1)
    Set b = x.Sheets(x.Charts(1).Name)
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
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    Set c = x.Charts.Add
    Set a = c
    Set b = x.Charts(1)
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
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    x.Excel4MacroSheets.Add
    Set a = x.Excel4MacroSheets(1)
    Set b = x.Excel4MacroSheets(1)
    Case_058 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_058 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_059() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    Set c = x.Excel4MacroSheets.Add
    Set a = c
    Set b = x.Excel4MacroSheets(1)
    Case_059 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_059 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_060() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    x.DialogSheets.Add
    Set a = x.DialogSheets(1)
    Set b = x.DialogSheets(1)
    Case_060 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_060 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_061() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = app.Workbooks.Add
    ThisWorkbook.Activate
    x.Modules.Add
    Set a = x.Modules(1)
    Set b = x.Modules(1)
    Case_061 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_061 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_062() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("A1")
    Set b = ws.Range("A1")
    Case_062 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_062 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_063() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Cells(1, 1)
    Set b = ws.Cells(1, 1)
    Case_063 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_063 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_064() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x
    Set b = x
    Case_064 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_064 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_065() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set c = x
    Set a = x
    Set b = c
    Case_065 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_065 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_066() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = app.ActiveCell
    Set b = app.ActiveCell
    Case_066 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_066 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_067() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = app.Selection
    Set b = app.Selection
    Case_067 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_067 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_068() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.UsedRange
    Set b = ws.UsedRange
    Case_068 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_068 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_069() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Cells
    Set b = ws.Cells
    Case_069 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_069 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_070() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Rows
    Set b = ws.Rows
    Case_070 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_070 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_071() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1:B2")
    Set a = x
    Set b = x.Cells
    Case_071 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_071 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_072() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1:B2")
    Set a = x
    Set b = x.Offset(0, 0)
    Case_072 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_072 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_073() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1:B2")
    Set a = x
    Set b = x.Areas(1)
    Case_073 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_073 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_074() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x
    Set b = x.Range("A1")
    Case_074 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_074 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_075() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x
    Set b = x.MergeArea
    Case_075 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_075 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_076() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Rows(1)
    Set a = x
    Set b = x.EntireRow
    Case_076 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_076 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_077() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1:B2")
    Set a = x.Rows
    Set b = x.Rows
    Case_077 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_077 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_078() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    For Each c In x
    Exit For
    Next
    Set a = x
    Set b = c
    Case_078 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_078 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_079() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1:B2")
    Set a = x.Areas
    Set b = x.Areas
    Case_079 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_079 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_080() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Font
    Set b = x.Font
    Case_080 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_080 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_081() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1").Font
    Set a = x
    Set b = x
    Case_081 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_081 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_082() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Interior
    Set b = x.Interior
    Case_082 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_082 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_083() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Borders
    Set b = x.Borders
    Case_083 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_083 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_084() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Borders(7)
    Set b = x.Borders(7)
    Case_084 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_084 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_085() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Style
    Set b = x.Style
    Case_085 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_085 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_086() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Validation
    Set b = x.Validation
    Case_086 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_086 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_087() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.FormatConditions
    Set b = x.FormatConditions
    Case_087 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_087 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_088() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Characters
    Set b = x.Characters
    Case_088 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_088 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_089() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Hyperlinks
    Set b = x.Hyperlinks
    Case_089 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_089 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_090() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Font.Parent
    Set b = x
    Case_090 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_090 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_091() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Interior.Parent
    Set b = x
    Case_091 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_091 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_092() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Borders.Parent
    Set b = x
    Case_092 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_092 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_093() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Validation.Parent
    Set b = x
    Case_093 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_093 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_094() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = ws.Range("A1")
    Set a = x.Areas.Parent
    Set b = x
    Case_094 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_094 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_095() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set c = ws.Range("A1").Font
    Set a = c.Parent
    Set b = c.Parent
    Case_095 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_095 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_096() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.PageSetup
    Set b = ws.PageSetup
    Case_096 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_096 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_097() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Outline
    Set b = ws.Outline
    Case_097 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_097 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_098() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Sort
    Set b = ws.Sort
    Case_098 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_098 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_099() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Tab
    Set b = ws.Tab
    Case_099 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_099 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_100() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Protection
    Set b = ws.Protection
    Case_100 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_100 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_101() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Shapes
    Set b = ws.Shapes
    Case_101 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_101 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_102() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ListObjects
    Set b = ws.ListObjects
    Case_102 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_102 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_103() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Hyperlinks
    Set b = ws.Hyperlinks
    Case_103 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_103 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_104() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Comments
    Set b = ws.Comments
    Case_104 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_104 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_105() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.ChartObjects
    Set b = ws.ChartObjects
    Case_105 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_105 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_106() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.Names("nm")
    Set b = wb.Names("nm")
    Case_106 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_106 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_107() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = app.Names("nm")
    Set b = app.Names("nm")
    Case_107 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_107 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_108() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.Names("nm")
    Set b = wb.Names.Item("NM")
    Case_108 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_108 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_109() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.Names("nm")
    Set b = ws.Range("A1").Name
    Case_109 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_109 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_110() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Range("A1").Name
    Set b = ws.Range("A1").Name
    Case_110 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_110 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_111() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Names("loc")
    Set b = ws.Names("loc")
    Case_111 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_111 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_112() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.Names("Sheet1!loc")
    Set b = ws.Names("loc")
    Case_112 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_112 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_113() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.Names("nm")
    Set b = ws.Names("loc")
    Case_113 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_113 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_114() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.Names("nm").Parent
    Set b = wb
    Case_114 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_114 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_115() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = ws.Names("loc").Parent
    Set b = ws
    Case_115 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_115 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_116() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set a = wb.Names("nm").RefersToRange
    Set b = wb.Names("nm").RefersToRange
    Case_116 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_116 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_117() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    For Each c In wb.Names
    If c.Name = "nm" Then Exit For
    Next
    Set a = c
    Set b = wb.Names("nm")
    Case_117 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_117 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_118() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = wb.Names.Add("nm_two", "=Sheet1!$C$1")
    Set a = x
    Set b = wb.Names("nm_two")
    Case_118 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_118 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_119() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = wb.Names("nm_two")
    wb.Names.Add "nm_two", "=Sheet1!$C$2"
    Set a = x
    Set b = wb.Names("nm_two")
    Case_119 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_119 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_120() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = wb.Names("nm_two")
    x.Name = "nm_three"
    Set a = x
    Set b = wb.Names("nm_three")
    Case_120 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_120 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_121() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = wb.Names("nm_three")
    x.Delete
    wb.Names.Add "nm_three", "=Sheet1!$C$3"
    Set a = x
    Set b = wb.Names("nm_three")
    Case_121 = CStr(a Is b)
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_121 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_122() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = CreateObject("Scripting.Dictionary")
    x.Add wb.Worksheets(1), 1
    Case_122 = CStr(x.Exists(wb.Worksheets(1)))
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_122 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_123() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = CreateObject("Scripting.Dictionary")
    x.Add ws.Range("A1"), 1
    Case_123 = CStr(x.Exists(ws.Range("A1")))
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_123 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
Public Function Case_124() As String
    On Error GoTo Fail
    Dim app As Object, wb As Object, ws As Object, a As Object, b As Object, c As Object, x As Object
    Set app = Application
    Set wb = ThisWorkbook
    Set ws = wb.Worksheets("Sheet1")
    Set x = CreateObject("Scripting.Dictionary")
    x.Add wb.Names("nm"), 1
    Case_124 = CStr(x.Exists(wb.Names("nm")))
    ThisWorkbook.Activate
    Exit Function
Fail:
    Case_124 = "#E " & Err.Number
    ThisWorkbook.Activate
End Function
