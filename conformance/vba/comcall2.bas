Attribute VB_Name = "ModComCall2"
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
    ElseIf IsError(v) Then
        Sh = CStr(v)
    Else
        Sh = TypeName(v) & " " & CStr(v)
    End If
End Function
Private Function LetOf(ByVal v As Variant) As String
    Dim x As Variant
    x = v
    LetOf = TypeName(v) & "/" & TypeName(x)
End Function
Function CdIxCell() As String
    On Error GoTo Fail
    CdIxCell = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 2, 1))
    Exit Function
Fail:
    CdIxCell = "#E " & Err.Number
End Function
Function CdIxCol() As String
    On Error GoTo Fail
    CdIxCol = Sh(WorksheetFunction.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 2))
    Exit Function
Fail:
    CdIxCol = "#E " & Err.Number
End Function
Function CdIxRow() As String
    On Error GoTo Fail
    CdIxRow = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 1, 0))
    Exit Function
Fail:
    CdIxRow = "#E " & Err.Number
End Function
Function CdIxRow2() As String
    On Error GoTo Fail
    CdIxRow2 = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 2))
    Exit Function
Fail:
    CdIxRow2 = "#E " & Err.Number
End Function
Function CdIxAll() As String
    On Error GoTo Fail
    CdIxAll = Sh(WorksheetFunction.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 0))
    Exit Function
Fail:
    CdIxAll = "#E " & Err.Number
End Function
Function CdIxOut() As String
    On Error GoTo Fail
    CdIxOut = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 5, 1))
    Exit Function
Fail:
    CdIxOut = "#E " & Err.Number
End Function
Function CdIxOutW() As String
    On Error GoTo Fail
    CdIxOutW = Sh(WorksheetFunction.Index(Worksheets("Sheet1").Range("A1:B2"), 5, 1))
    Exit Function
Fail:
    CdIxOutW = "#E " & Err.Number
End Function
Function CdIxNa() As String
    On Error GoTo Fail
    CdIxNa = Sh(WorksheetFunction.Index(Worksheets("Sheet1").Range("G1:G2"), 1, 1))
    Exit Function
Fail:
    CdIxNa = "#E " & Err.Number
End Function
Function CdIxNaA() As String
    On Error GoTo Fail
    CdIxNaA = Sh(Application.Index(Worksheets("Sheet1").Range("G1:G2"), 1, 1))
    Exit Function
Fail:
    CdIxNaA = "#E " & Err.Number
End Function
Function CdIxEmpty() As String
    On Error GoTo Fail
    CdIxEmpty = Sh(Application.Index(Worksheets("Sheet1").Range("C1:C2"), 1, 1))
    Exit Function
Fail:
    CdIxEmpty = "#E " & Err.Number
End Function
Function CdIxWhole() As String
    On Error GoTo Fail
    CdIxWhole = Sh(Application.Index(Worksheets("Sheet1").Range("A:B"), 0, 1))
    Exit Function
Fail:
    CdIxWhole = "#E " & Err.Number
End Function
Function CdIxWholeR() As String
    On Error GoTo Fail
    CdIxWholeR = Sh(Application.Index(Worksheets("Sheet1").Range("1:3"), 2, 0))
    Exit Function
Fail:
    CdIxWholeR = "#E " & Err.Number
End Function
Function CdIxArea2() As String
    On Error GoTo Fail
    CdIxArea2 = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2,D1:E2"), 1, 2, 2))
    Exit Function
Fail:
    CdIxArea2 = "#E " & Err.Number
End Function
Function CdIxArea1() As String
    On Error GoTo Fail
    CdIxArea1 = Sh(WorksheetFunction.Index(Worksheets("Sheet1").Range("A1:B2,D1:E2"), 0, 1))
    Exit Function
Fail:
    CdIxArea1 = "#E " & Err.Number
End Function
Function CdIxArea3() As String
    On Error GoTo Fail
    CdIxArea3 = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2,D1:E2"), 1, 1, 3))
    Exit Function
Fail:
    CdIxArea3 = "#E " & Err.Number
End Function
Function CdIxSheet2() As String
    On Error GoTo Fail
    CdIxSheet2 = Sh(Application.Index(Worksheets("Sheet2").Range("A1:B2"), 2, 2).Parent.Name)
    Exit Function
Fail:
    CdIxSheet2 = "#E " & Err.Number
End Function
Function CdIxNest() As String
    On Error GoTo Fail
    CdIxNest = Sh(Application.Index(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 2), 2))
    Exit Function
Fail:
    CdIxNest = "#E " & Err.Number
End Function
Function CdChR() As String
    On Error GoTo Fail
    CdChR = Sh(Application.Choose(2, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdChR = "#E " & Err.Number
End Function
Function CdChW() As String
    On Error GoTo Fail
    CdChW = Sh(WorksheetFunction.Choose(1, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdChW = "#E " & Err.Number
End Function
Function CdChS() As String
    On Error GoTo Fail
    CdChS = Sh(WorksheetFunction.Choose(2, Worksheets("Sheet1").Range("A1:A2"), "x"))
    Exit Function
Fail:
    CdChS = "#E " & Err.Number
End Function
Function CdChU() As String
    On Error GoTo Fail
    CdChU = Sh(Application.Choose(1, Worksheets("Sheet1").Range("A1:B2,D1:E2")))
    Exit Function
Fail:
    CdChU = "#E " & Err.Number
End Function
Function CdChArr() As String
    On Error GoTo Fail
    CdChArr = Sh(Application.Choose(Array(1, 2), Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdChArr = "#E " & Err.Number
End Function
Function CdChOut() As String
    On Error GoTo Fail
    CdChOut = Sh(Application.Choose(3, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdChOut = "#E " & Err.Number
End Function
Function CdXl() As String
    On Error GoTo Fail
    CdXl = Sh(Application.XLookup(3, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdXl = "#E " & Err.Number
End Function
Function CdXlW() As String
    On Error GoTo Fail
    CdXlW = Sh(WorksheetFunction.XLookup(1, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("A1:B2")))
    Exit Function
Fail:
    CdXlW = "#E " & Err.Number
End Function
Function CdXlNf() As String
    On Error GoTo Fail
    CdXlNf = Sh(Application.XLookup(9, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2"), Worksheets("Sheet1").Range("D1")))
    Exit Function
Fail:
    CdXlNf = "#E " & Err.Number
End Function
Function CdXlNfV() As String
    On Error GoTo Fail
    CdXlNfV = Sh(Application.XLookup(9, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2"), "none"))
    Exit Function
Fail:
    CdXlNfV = "#E " & Err.Number
End Function
Function CdXlMiss() As String
    On Error GoTo Fail
    CdXlMiss = Sh(Application.XLookup(9, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdXlMiss = "#E " & Err.Number
End Function
Function CdXlArr() As String
    On Error GoTo Fail
    CdXlArr = Sh(Application.XLookup(3, Array(1, 3), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdXlArr = "#E " & Err.Number
End Function
Function CdXlArr2() As String
    On Error GoTo Fail
    CdXlArr2 = Sh(Application.XLookup(3, Worksheets("Sheet1").Range("A1:A2"), Array(5, 6)))
    Exit Function
Fail:
    CdXlArr2 = "#E " & Err.Number
End Function
Function CdSg() As String
    On Error GoTo Fail
    CdSg = Sh(Application.Single(Worksheets("Sheet1").Range("A1:A2")))
    Exit Function
Fail:
    CdSg = "#E " & Err.Number
End Function
Function CdSgOne() As String
    On Error GoTo Fail
    CdSgOne = Sh(WorksheetFunction.Single(Worksheets("Sheet1").Range("B2")))
    Exit Function
Fail:
    CdSgOne = "#E " & Err.Number
End Function
Function CdIfE() As String
    On Error GoTo Fail
    CdIfE = Sh(Application.IfError(Worksheets("Sheet1").Range("B1"), 0))
    Exit Function
Fail:
    CdIfE = "#E " & Err.Number
End Function
Function CdIfNa() As String
    On Error GoTo Fail
    CdIfNa = Sh(WorksheetFunction.IfNa(Worksheets("Sheet1").Range("B1"), 0))
    Exit Function
Fail:
    CdIfNa = "#E " & Err.Number
End Function
Function CdLk() As String
    On Error GoTo Fail
    CdLk = Sh(Application.Lookup(3, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdLk = "#E " & Err.Number
End Function
Function CdVLk() As String
    On Error GoTo Fail
    CdVLk = Sh(WorksheetFunction.VLookup(1, Worksheets("Sheet1").Range("A1:B2"), 2, False))
    Exit Function
Fail:
    CdVLk = "#E " & Err.Number
End Function
Function CdOffs() As String
    On Error GoTo Fail
    CdOffs = Sh(Application.Offset(Worksheets("Sheet1").Range("A1:A2"), 0, 1))
    Exit Function
Fail:
    CdOffs = "#E " & Err.Number
End Function
Function CdIndir() As String
    On Error GoTo Fail
    CdIndir = Sh(Application.Indirect("A1:B2"))
    Exit Function
Fail:
    CdIndir = "#E " & Err.Number
End Function
Function CdAppIf() As String
    On Error GoTo Fail
    CdAppIf = Sh(Application.If(True, Worksheets("Sheet1").Range("A1:A2"), Worksheets("Sheet1").Range("B1:B2")))
    Exit Function
Fail:
    CdAppIf = "#E " & Err.Number
End Function
Function CdIfs() As String
    On Error GoTo Fail
    CdIfs = Sh(Application.Ifs(True, Worksheets("Sheet1").Range("A1:A2")))
    Exit Function
Fail:
    CdIfs = "#E " & Err.Number
End Function
Function CdSwitch() As String
    On Error GoTo Fail
    CdSwitch = Sh(Application.Switch(1, 1, Worksheets("Sheet1").Range("A1:A2")))
    Exit Function
Fail:
    CdSwitch = "#E " & Err.Number
End Function
Function CdIxArrV() As String
    On Error GoTo Fail
    CdIxArrV = Sh(Application.Index(Array(5, 6, 7), 2))
    Exit Function
Fail:
    CdIxArrV = "#E " & Err.Number
End Function
Function CdAdd() As String
    On Error GoTo Fail
    CdAdd = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 2, 1) + 10)
    Exit Function
Fail:
    CdAdd = "#E " & Err.Number
End Function
Function CdEq() As String
    On Error GoTo Fail
    CdEq = Sh((Application.Index(Worksheets("Sheet1").Range("A1:B2"), 2, 1) = 3))
    Exit Function
Fail:
    CdEq = "#E " & Err.Number
End Function
Function CdCnt() As String
    On Error GoTo Fail
    CdCnt = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 1).Count)
    Exit Function
Fail:
    CdCnt = "#E " & Err.Number
End Function
Function CdOff() As String
    On Error GoTo Fail
    CdOff = Sh(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 1).Offset(1, 0).Address)
    Exit Function
Fail:
    CdOff = "#E " & Err.Number
End Function
Function CdCstr() As String
    On Error GoTo Fail
    CdCstr = Sh(CStr(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 1, 2)))
    Exit Function
Fail:
    CdCstr = "#E " & Err.Number
End Function
Function CdIsErr() As String
    On Error GoTo Fail
    CdIsErr = Sh(IsError(Application.Index(Worksheets("Sheet1").Range("G1:G2"), 1, 1)))
    Exit Function
Fail:
    CdIsErr = "#E " & Err.Number
End Function
Function CdIsEmp() As String
    On Error GoTo Fail
    CdIsEmp = Sh(IsEmpty(Application.Index(Worksheets("Sheet1").Range("C1:C2"), 1, 1)))
    Exit Function
Fail:
    CdIsEmp = "#E " & Err.Number
End Function
Function CdIsNum() As String
    On Error GoTo Fail
    CdIsNum = Sh(IsNumeric(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 1, 1)))
    Exit Function
Fail:
    CdIsNum = "#E " & Err.Number
End Function
Function CdIsArr() As String
    On Error GoTo Fail
    CdIsArr = Sh(IsArray(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 1)))
    Exit Function
Fail:
    CdIsArr = "#E " & Err.Number
End Function
Function CdVarT() As String
    On Error GoTo Fail
    CdVarT = Sh(VarType(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 1)))
    Exit Function
Fail:
    CdVarT = "#E " & Err.Number
End Function
Function CdSumIx() As String
    On Error GoTo Fail
    CdSumIx = Sh(WorksheetFunction.Sum(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 2)))
    Exit Function
Fail:
    CdSumIx = "#E " & Err.Number
End Function
Function CdSumU() As String
    On Error GoTo Fail
    CdSumU = Sh(WorksheetFunction.Sum(Worksheets("Sheet1").Range("A1:A2,B1")))
    Exit Function
Fail:
    CdSumU = "#E " & Err.Number
End Function
Function CdCntU() As String
    On Error GoTo Fail
    CdCntU = Sh(Application.Count(Worksheets("Sheet1").Range("A1:B2,D1:E2")))
    Exit Function
Fail:
    CdCntU = "#E " & Err.Number
End Function
Function CdLetV() As String
    On Error GoTo Fail
    CdLetV = Sh(LetOf(Application.Index(Worksheets("Sheet1").Range("A1:B2"), 0, 1)))
    Exit Function
Fail:
    CdLetV = "#E " & Err.Number
End Function
Sub CdMore()
    Worksheets("Sheet1").Activate
    Worksheets("Sheet1").Range("A1").Select
    Worksheets("Sheet1").Cells(1, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:A2")))
    Worksheets("Sheet1").Range("A1").Select
    Worksheets("Sheet1").Cells(2, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:B1")))
    Worksheets("Sheet1").Range("A1").Select
    Worksheets("Sheet1").Cells(3, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:B2")))
    Worksheets("Sheet1").Range("D2").Select
    Worksheets("Sheet1").Cells(4, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:A2")))
    Worksheets("Sheet1").Range("D2").Select
    Worksheets("Sheet1").Cells(5, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:B1")))
    Worksheets("Sheet1").Range("D2").Select
    Worksheets("Sheet1").Cells(6, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:B2")))
    Worksheets("Sheet1").Range("B2").Select
    Worksheets("Sheet1").Cells(7, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:A2")))
    Worksheets("Sheet1").Range("B2").Select
    Worksheets("Sheet1").Cells(8, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:B1")))
    Worksheets("Sheet1").Range("B2").Select
    Worksheets("Sheet1").Cells(9, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:B2")))
    Worksheets("Sheet1").Range("A5").Select
    Worksheets("Sheet1").Cells(10, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:A2")))
    Worksheets("Sheet1").Range("A5").Select
    Worksheets("Sheet1").Cells(11, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:B1")))
    Worksheets("Sheet1").Range("A5").Select
    Worksheets("Sheet1").Cells(12, 15).Value = Sh(Application.Single(Worksheets("Sheet1").Range("A1:B2")))
    Dim b As Workbook, x As Object
    Set b = Workbooks.Add
    b.Worksheets(1).Range("A2").Value = 55
    ThisWorkbook.Activate
    Set x = Application.Index(b.Worksheets(1).Range("A1:A2"), 2)
    Worksheets("Sheet1").Range("P1").Value = CStr(x.Value) & " " & (x.Parent.Parent.Name = b.Name)
    Worksheets("Sheet1").Range("P2").Value = CStr(WorksheetFunction.Sum(b.Worksheets(1).Range("A1:A2")))
    b.Close False
    Set x = Application.Index(Worksheets("Sheet1").Range("A1:B2"), 2, 2)
    x.Value = 44
    Worksheets("Sheet1").Range("P3").Value = Worksheets("Sheet1").Range("B2").Value
    x.Value = 4
End Sub
Sub CdAll()
    Worksheets("Sheet1").Cells(1, 11).Value = CdIxCell()
    Worksheets("Sheet1").Cells(2, 11).Value = CdIxCol()
    Worksheets("Sheet1").Cells(3, 11).Value = CdIxRow()
    Worksheets("Sheet1").Cells(4, 11).Value = CdIxRow2()
    Worksheets("Sheet1").Cells(5, 11).Value = CdIxAll()
    Worksheets("Sheet1").Cells(6, 11).Value = CdIxOut()
    Worksheets("Sheet1").Cells(7, 11).Value = CdIxOutW()
    Worksheets("Sheet1").Cells(8, 11).Value = CdIxNa()
    Worksheets("Sheet1").Cells(9, 11).Value = CdIxNaA()
    Worksheets("Sheet1").Cells(10, 11).Value = CdIxEmpty()
    Worksheets("Sheet1").Cells(11, 11).Value = CdIxWhole()
    Worksheets("Sheet1").Cells(12, 11).Value = CdIxWholeR()
    Worksheets("Sheet1").Cells(13, 11).Value = CdIxArea2()
    Worksheets("Sheet1").Cells(14, 11).Value = CdIxArea1()
    Worksheets("Sheet1").Cells(15, 11).Value = CdIxArea3()
    Worksheets("Sheet1").Cells(16, 11).Value = CdIxSheet2()
    Worksheets("Sheet1").Cells(17, 11).Value = CdIxNest()
    Worksheets("Sheet1").Cells(18, 11).Value = CdChR()
    Worksheets("Sheet1").Cells(19, 11).Value = CdChW()
    Worksheets("Sheet1").Cells(20, 11).Value = CdChS()
    Worksheets("Sheet1").Cells(21, 11).Value = CdChU()
    Worksheets("Sheet1").Cells(22, 11).Value = CdChArr()
    Worksheets("Sheet1").Cells(23, 11).Value = CdChOut()
    Worksheets("Sheet1").Cells(24, 11).Value = CdXl()
    Worksheets("Sheet1").Cells(25, 11).Value = CdXlW()
    Worksheets("Sheet1").Cells(26, 11).Value = CdXlNf()
    Worksheets("Sheet1").Cells(27, 11).Value = CdXlNfV()
    Worksheets("Sheet1").Cells(28, 11).Value = CdXlMiss()
    Worksheets("Sheet1").Cells(29, 11).Value = CdXlArr()
    Worksheets("Sheet1").Cells(30, 11).Value = CdXlArr2()
    Worksheets("Sheet1").Cells(31, 11).Value = CdSg()
    Worksheets("Sheet1").Cells(32, 11).Value = CdSgOne()
    Worksheets("Sheet1").Cells(33, 11).Value = CdIfE()
    Worksheets("Sheet1").Cells(34, 11).Value = CdIfNa()
    Worksheets("Sheet1").Cells(35, 11).Value = CdLk()
    Worksheets("Sheet1").Cells(36, 11).Value = CdVLk()
    Worksheets("Sheet1").Cells(37, 11).Value = CdOffs()
    Worksheets("Sheet1").Cells(38, 11).Value = CdIndir()
    Worksheets("Sheet1").Cells(39, 11).Value = CdAppIf()
    Worksheets("Sheet1").Cells(40, 11).Value = CdIfs()
    Worksheets("Sheet1").Cells(41, 11).Value = CdSwitch()
    Worksheets("Sheet1").Cells(42, 11).Value = CdIxArrV()
    Worksheets("Sheet1").Cells(43, 11).Value = CdAdd()
    Worksheets("Sheet1").Cells(44, 11).Value = CdEq()
    Worksheets("Sheet1").Cells(45, 11).Value = CdCnt()
    Worksheets("Sheet1").Cells(46, 11).Value = CdOff()
    Worksheets("Sheet1").Cells(47, 11).Value = CdCstr()
    Worksheets("Sheet1").Cells(48, 11).Value = CdIsErr()
    Worksheets("Sheet1").Cells(49, 11).Value = CdIsEmp()
    Worksheets("Sheet1").Cells(50, 11).Value = CdIsNum()
    Worksheets("Sheet1").Cells(51, 11).Value = CdIsArr()
    Worksheets("Sheet1").Cells(52, 11).Value = CdVarT()
    Worksheets("Sheet1").Cells(53, 11).Value = CdSumIx()
    Worksheets("Sheet1").Cells(54, 11).Value = CdSumU()
    Worksheets("Sheet1").Cells(55, 11).Value = CdCntU()
    Worksheets("Sheet1").Cells(56, 11).Value = CdLetV()
End Sub
