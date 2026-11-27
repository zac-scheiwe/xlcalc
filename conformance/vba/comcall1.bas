Attribute VB_Name = "ModComCall1"
Option Explicit
Private Function Sh(v As Variant) As String
    Dim s As String, x As Variant, n As Long
    If IsObject(v) Then
        Sh = "<" & TypeName(v) & " " & v.Address & ">"
    ElseIf IsArray(v) Then
        n = -1
        On Error Resume Next
        n = UBound(v, 2)
        On Error GoTo 0
        s = TypeName(v) & " " & UBound(v, 1)
        If n >= 0 Then s = s & "x" & n
        s = s & ":"
        For Each x In v
            s = s & " " & Sh(x)
        Next x
        Sh = s
    Else
        Sh = TypeName(v) & " " & CStr(v)
    End If
End Function
Function CcTr() As String
    On Error GoTo Fail
    CcTr = Sh(Application.Transpose(Worksheets("Sheet1").Range("A1:B2")))
    Exit Function
Fail:
    CcTr = "#E " & Err.Number
End Function
Function CcWTr() As String
    On Error GoTo Fail
    CcWTr = Sh(WorksheetFunction.Transpose(Array(1, 2, 3)))
    Exit Function
Fail:
    CcWTr = "#E " & Err.Number
End Function
Function CcMM() As String
    On Error GoTo Fail
    CcMM = Sh(WorksheetFunction.MMult(Worksheets("Sheet1").Range("A1:B2"), Worksheets("Sheet1").Range("A1:B2")))
    Exit Function
Fail:
    CcMM = "#E " & Err.Number
End Function
Function CcIdx() As String
    On Error GoTo Fail
    CcIdx = Sh(Application.Index(Application.Transpose(Array(5, 6, 7)), 0, 1))
    Exit Function
Fail:
    CcIdx = "#E " & Err.Number
End Function
Function CcIdxR() As String
    On Error GoTo Fail
    CcIdxR = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 1))
    Exit Function
Fail:
    CcIdxR = "#E " & Err.Number
End Function
Function CcFreq() As String
    On Error GoTo Fail
    CcFreq = Sh(WorksheetFunction.Frequency(Worksheets("Sheet1").Range("A1:B2"), Array(2)))
    Exit Function
Fail:
    CcFreq = "#E " & Err.Number
End Function
Function CcTrB() As String
    On Error GoTo Fail
    CcTrB = Sh(Application.Transpose(Application.Transpose(Worksheets("Sheet1").Range("A1:C1"))))
    Exit Function
Fail:
    CcTrB = "#E " & Err.Number
End Function
Function CcEvTr() As String
    On Error GoTo Fail
    CcEvTr = Sh(Application.Evaluate("TRANSPOSE(A1:B2)"))
    Exit Function
Fail:
    CcEvTr = "#E " & Err.Number
End Function
Function CcEvRow() As String
    On Error GoTo Fail
    CcEvRow = Sh(Application.Evaluate("ROW(A1:A3)"))
    Exit Function
Fail:
    CcEvRow = "#E " & Err.Number
End Function
Function CcEvMul() As String
    On Error GoTo Fail
    CcEvMul = Sh(Application.Evaluate("A1:B2*10"))
    Exit Function
Fail:
    CcEvMul = "#E " & Err.Number
End Function
Function CcEvAbs() As String
    On Error GoTo Fail
    CcEvAbs = Sh(Application.Evaluate("ABS(A1:B2)"))
    Exit Function
Fail:
    CcEvAbs = "#E " & Err.Number
End Function
Function CcEvIf() As String
    On Error GoTo Fail
    CcEvIf = Sh(Application.Evaluate("IF(A1:B2>2,A1:B2,0)"))
    Exit Function
Fail:
    CcEvIf = "#E " & Err.Number
End Function
Function CcEvSum() As String
    On Error GoTo Fail
    CcEvSum = Sh(Application.Evaluate("SUM(ABS(A1:B2))"))
    Exit Function
Fail:
    CcEvSum = "#E " & Err.Number
End Function
Function CcWsEv() As String
    On Error GoTo Fail
    CcWsEv = Sh(Worksheets("Sheet1").Evaluate("MMULT(A1:B2,A1:B2)"))
    Exit Function
Fail:
    CcWsEv = "#E " & Err.Number
End Function
Function CcVL() As String
    On Error GoTo Fail
    CcVL = Sh(WorksheetFunction.VLookup(1, Worksheets("Sheet1").Range("A1:C1"), 3, False))
    Exit Function
Fail:
    CcVL = "#E " & Err.Number
End Function
Function CcIdxE() As String
    On Error GoTo Fail
    CcIdxE = Sh(Application.Index(Application.Transpose(Application.Transpose(Worksheets("Sheet1").Range("A1:C1"))), 3))
    Exit Function
Fail:
    CcIdxE = "#E " & Err.Number
End Function
Function CcTrOne() As String
    On Error GoTo Fail
    CcTrOne = Sh(Application.Transpose(Worksheets("Sheet1").Range("C1")))
    Exit Function
Fail:
    CcTrOne = "#E " & Err.Number
End Function
Sub CcSetup()
    Worksheets("Sheet1").Range("D1").Formula = "=1+1"
    Worksheets("Sheet1").Range("D3:E3").FormulaArray = "=A1:B1*1"
    Worksheets("Sheet1").Range("D5").Formula2 = "=SEQUENCE(1)"
End Sub
Sub CcAt(ByVal at As String, ByVal col As Long)
    Worksheets("Sheet1").Activate
    Worksheets("Sheet1").Range(at).Select
    Worksheets("Sheet1").Cells(1, col).Value = CcTr()
    Worksheets("Sheet1").Cells(2, col).Value = CcWTr()
    Worksheets("Sheet1").Cells(3, col).Value = CcMM()
    Worksheets("Sheet1").Cells(4, col).Value = CcIdx()
    Worksheets("Sheet1").Cells(5, col).Value = CcIdxR()
    Worksheets("Sheet1").Cells(6, col).Value = CcFreq()
    Worksheets("Sheet1").Cells(7, col).Value = CcTrB()
    Worksheets("Sheet1").Cells(8, col).Value = CcEvTr()
    Worksheets("Sheet1").Cells(9, col).Value = CcEvRow()
    Worksheets("Sheet1").Cells(10, col).Value = CcEvMul()
    Worksheets("Sheet1").Cells(11, col).Value = CcEvAbs()
    Worksheets("Sheet1").Cells(12, col).Value = CcEvIf()
    Worksheets("Sheet1").Cells(13, col).Value = CcEvSum()
    Worksheets("Sheet1").Cells(14, col).Value = CcWsEv()
    Worksheets("Sheet1").Cells(15, col).Value = CcVL()
    Worksheets("Sheet1").Cells(16, col).Value = CcIdxE()
    Worksheets("Sheet1").Cells(17, col).Value = CcTrOne()
End Sub
