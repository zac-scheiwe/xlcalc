Attribute VB_Name = "MacroEdit"
Option Explicit

Public Calls As Long

Public Function Dbl(ByVal x As Variant) As Variant
    On Error GoTo EH
    Calls = Calls + 1
    Dbl = x * 2
    Exit Function
EH:
    Dbl = "err " & Err.Number
End Function

Public Function GetCalls() As Variant
    On Error GoTo EH
    GetCalls = Calls
    Calls = 0
    Exit Function
EH:
    GetCalls = "err " & Err.Number
End Function

Public Function FillSquares(ByVal n As Long) As Variant
    On Error GoTo EH
    Dim i As Long
    For i = 1 To n
        EvLog.SafeWrite Worksheets("Sheet1").Cells(i, 2), i * i
    Next i
    FillSquares = n
    Exit Function
EH:
    FillSquares = "err " & Err.Number
End Function

Public Function SumDown() As Variant
    On Error GoTo EH
    Dim c As Range, t As Double
    For Each c In Worksheets("Sheet1").Range("B1:B5").Cells
        If IsNumeric(c.Value2) Then t = t + c.Value2
    Next c
    EvLog.SafeWrite Worksheets("Sheet1").Range("B7"), t
    SumDown = t
    Exit Function
EH:
    SumDown = "err " & Err.Number
End Function

Public Function WriteThenRead(ByVal v As Variant) As Variant
    On Error GoTo EH
    EvLog.SafeWrite Worksheets("Sheet1").Range("D1"), v
    WriteThenRead = CStr(Worksheets("Sheet1").Range("E1").Value2) & ":" & Calls
    Exit Function
EH:
    WriteThenRead = "err " & Err.Number
End Function

Public Function WriteManualCalc(ByVal v As Variant) As Variant
    On Error GoTo EH
    Application.Calculation = xlCalculationManual
    EvLog.SafeWrite Worksheets("Sheet1").Range("D1"), v
    WriteManualCalc = CStr(Worksheets("Sheet1").Range("E1").Value2) & ":" & Calls
    Application.Calculate
    WriteManualCalc = WriteManualCalc & ":" & CStr(Worksheets("Sheet1").Range("E1").Value2) & ":" & Calls
    Application.Calculation = xlCalculationAutomatic
    Exit Function
EH:
    Application.Calculation = xlCalculationAutomatic
    WriteManualCalc = "err " & Err.Number
End Function

Public Function RecCopyPaste() As Variant
    On Error GoTo EH
    If Not EvLog.Safe(Worksheets("Sheet1").Range("G1:G3")) Then Exit Function
    Worksheets("Sheet1").Activate
    Range("A1:A3").Select
    Selection.Copy
    Range("G1").Select
    If EvLog.Safe(Range("G1:G3")) Then ActiveSheet.Paste
    Application.CutCopyMode = False
    RecCopyPaste = Selection.Address(External:=False) & ":" & ActiveCell.Address(External:=False)
    Exit Function
EH:
    RecCopyPaste = "err " & Err.Number
End Function

Public Function RecFormula() As Variant
    On Error GoTo EH
    Worksheets("Sheet1").Activate
    Range("H1").Select
    EvLog.SafeFormula ActiveCell, "=A1+A2"
    Range("H2").Select
    EvLog.SafeFormula ActiveCell, "=H1*2"
    Selection.Offset(1, 0).Select
    EvLog.SafeFormat ActiveCell, "0.00"
    EvLog.SafeWrite ActiveCell, 1 / 3
    RecFormula = ActiveCell.Address(External:=False) & ":" & ActiveCell.Text
    Exit Function
EH:
    RecFormula = "err " & Err.Number
End Function

Public Function RecClear() As Variant
    On Error GoTo EH
    Worksheets("Sheet1").Activate
    Range("A4:A5").Select
    If EvLog.Safe(Selection) Then Selection.ClearContents
    RecClear = Selection.Address(External:=False)
    Exit Function
EH:
    RecClear = "err " & Err.Number
End Function

Public Function RecInsertDelete() As Variant
    On Error GoTo EH
    Worksheets("Sheet1").Activate
    Rows("3:3").Select
    If EvLog.Safe(Selection) Then Selection.Insert Shift:=xlDown
    Rows("1:1").Select
    If EvLog.Safe(Selection) Then Selection.Delete Shift:=xlUp
    RecInsertDelete = Selection.Address(External:=False)
    Exit Function
EH:
    RecInsertDelete = "err " & Err.Number
End Function

Public Function RecSort() As Variant
    On Error GoTo EH
    Dim r As Range
    Set r = Worksheets("Sheet1").Range("J1:J5")
    If EvLog.Safe(r) Then
        r.Sort Key1:=Worksheets("Sheet1").Range("J1"), Order1:=xlDescending, Header:=xlNo
    End If
    RecSort = r.Cells(1, 1).Value2 & "," & r.Cells(5, 1).Value2
    Exit Function
EH:
    RecSort = "err " & Err.Number
End Function

Public Function RecAutoFill() As Variant
    On Error GoTo EH
    Dim r As Range
    Set r = Worksheets("Sheet1").Range("L1:L8")
    If EvLog.Safe(r) Then Worksheets("Sheet1").Range("L1:L2").AutoFill Destination:=r
    RecAutoFill = r.Cells(8, 1).Value2
    Exit Function
EH:
    RecAutoFill = "err " & Err.Number
End Function

Public Function UsedRangeInfo() As Variant
    On Error GoTo EH
    UsedRangeInfo = Worksheets("Sheet1").UsedRange.Address(External:=False) & ":" & Worksheets("S2").UsedRange.Address(External:=False)
    Exit Function
EH:
    UsedRangeInfo = "err " & Err.Number
End Function

Public Function WriteOtherSheet() As Variant
    On Error GoTo EH
    EvLog.SafeWrite Worksheets("S2").Range("C1"), "from macro"
    EvLog.SafeFormula Worksheets("S2").Range("C2"), "=Sheet1!A1+1"
    WriteOtherSheet = Worksheets("S2").Range("C2").Value2
    Exit Function
EH:
    WriteOtherSheet = "err " & Err.Number
End Function

Public Function RefusedWrite() As Variant
    On Error GoTo EH
    RefusedWrite = EvLog.SafeWrite(Worksheets("Sheet1").Range("Z1:Z2"), 1)
    Exit Function
EH:
    RefusedWrite = "err " & Err.Number
End Function

Public Function CellTypes() As Variant
    On Error GoTo EH
    EvLog.SafeWrite Worksheets("Sheet1").Range("N1"), "text"
    EvLog.SafeWrite Worksheets("Sheet1").Range("N2"), 12.5
    EvLog.SafeWrite Worksheets("Sheet1").Range("N3"), True
    EvLog.SafeWrite Worksheets("Sheet1").Range("N4"), CDate(36526)
    EvLog.SafeWrite Worksheets("Sheet1").Range("N5"), Empty
    EvLog.SafeWrite Worksheets("Sheet1").Range("N6"), CVErr(2042)
    EvLog.SafeWrite Worksheets("Sheet1").Range("N7"), "=1+1"
    EvLog.SafeWrite Worksheets("Sheet1").Range("N8"), "'=text"
    EvLog.SafeWrite Worksheets("Sheet1").Range("N9"), "1/2/2020"
    EvLog.SafeWrite Worksheets("Sheet1").Range("N10"), CCur(1.5)
    CellTypes = TypeName(Worksheets("Sheet1").Range("N4").Value2) & ":" & TypeName(Worksheets("Sheet1").Range("N4").Value) & ":" & TypeName(Worksheets("Sheet1").Range("N10").Value)
    Exit Function
EH:
    CellTypes = "err " & Err.Number
End Function

Public Function ArrayWrite() As Variant
    On Error GoTo EH
    Dim a(1 To 2, 1 To 2) As Variant
    a(1, 1) = 1: a(1, 2) = "x": a(2, 1) = True: a(2, 2) = 2.5
    EvLog.SafeWrite Worksheets("Sheet1").Range("P1:Q2"), a
    ArrayWrite = Worksheets("Sheet1").Range("P1:Q2").Cells(2, 2).Value2
    Exit Function
EH:
    ArrayWrite = "err " & Err.Number
End Function
