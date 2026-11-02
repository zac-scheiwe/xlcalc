Attribute VB_Name = "MacroRun"
Option Explicit

Public Total As Long
Public Seen As String

Public Function FnOne(ByVal a As Variant) As Variant
    On Error GoTo EH
    FnOne = "FnOne:" & TypeName(a) & ":" & CStr(a)
    Exit Function
EH:
    FnOne = "err " & Err.Number
End Function

Public Sub SubNoRet()
    On Error GoTo EH
    Total = Total + 1
    Exit Sub
EH:
    Total = -Err.Number
End Sub

Public Function GetTotal() As Variant
    On Error GoTo EH
    GetTotal = Total
    Exit Function
EH:
    GetTotal = "err " & Err.Number
End Function

Private Sub PrivSub()
    On Error GoTo EH
    Total = Total + 100
    Exit Sub
EH:
    Total = -Err.Number
End Sub

Private Function PrivFn() As Variant
    On Error GoTo EH
    PrivFn = "PrivFn"
    Exit Function
EH:
    PrivFn = "err " & Err.Number
End Function

Public Function Dup() As Variant
    On Error GoTo EH
    Dup = "MacroRun.Dup"
    Exit Function
EH:
    Dup = "err " & Err.Number
End Function

Public Function OptFn(ByVal a As Variant, Optional ByVal b As Variant = 7) As Variant
    On Error GoTo EH
    OptFn = TypeName(a) & ":" & TypeName(b) & ":" & CStr(b)
    Exit Function
EH:
    OptFn = "err " & Err.Number
End Function

Public Function OptStr(Optional ByVal s As String = "dflt", Optional ByVal n As Long = 3) As Variant
    On Error GoTo EH
    OptStr = s & "/" & n
    Exit Function
EH:
    OptStr = "err " & Err.Number
End Function

Public Function PaFn(ByVal a As Variant, ParamArray p() As Variant) As Variant
    On Error GoTo EH
    Dim i As Long, s As String
    For i = LBound(p) To UBound(p)
        s = s & TypeName(p(i)) & ","
    Next i
    PaFn = a & ":" & (UBound(p) - LBound(p) + 1) & ":" & s
    Exit Function
EH:
    PaFn = "err " & Err.Number
End Function

Public Function ByRefFn(ByRef x As Long) As Variant
    On Error GoTo EH
    x = x + 1
    ByRefFn = x
    Exit Function
EH:
    ByRefFn = "err " & Err.Number
End Function

Public Function ByRefVar(ByRef x As Variant) As Variant
    On Error GoTo EH
    x = x & "!"
    ByRefVar = x
    Exit Function
EH:
    ByRefVar = "err " & Err.Number
End Function

Public Function ArgMany(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30) As Variant
    On Error GoTo EH
    ArgMany = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 + a26 + a27 + a28 + a29 + a30
    Exit Function
EH:
    ArgMany = "err " & Err.Number
End Function

Public Function ArrFn(ByVal a As Variant) As Variant
    On Error GoTo EH
    ArrFn = TypeName(a) & ":" & LBound(a, 1) & "-" & UBound(a, 1) & ":" & LBound(a, 2) & "-" & UBound(a, 2)
    Exit Function
EH:
    ArrFn = "err " & Err.Number
End Function

Public Function RngFn(ByVal r As Range) As Variant
    On Error GoTo EH
    RngFn = r.Address(External:=False) & ":" & r.Cells.Count
    Exit Function
EH:
    RngFn = "err " & Err.Number
End Function

Public Function CallerFn() As Variant
    On Error GoTo EH
    CallerFn = Application.Caller
    Exit Function
EH:
    CallerFn = "err " & Err.Number
End Function

Public Function CallerType() As Variant
    On Error GoTo EH
    CallerType = TypeName(Application.Caller)
    Exit Function
EH:
    CallerType = "err " & Err.Number
End Function

Public Function ThrowFn(ByVal n As Long) As Variant
    On Error GoTo EH
    Err.Raise n, "MacroRun", "raised " & n
    ThrowFn = "not reached"
    Exit Function
EH:
    ThrowFn = Array(Err.Number, Err.Description, Err.Source)
End Function

Public Function CallsOther(ByVal a As Variant) As Variant
    On Error GoTo EH
    CallsOther = FnOne(a) & "+" & Application.Run("FnOne", a)
    Exit Function
EH:
    CallsOther = "err " & Err.Number
End Function

Public Function WhoAmI() As Variant
    On Error GoTo EH
    WhoAmI = Application.ActiveWorkbook.Name & ":" & ThisWorkbook.Name
    Exit Function
EH:
    WhoAmI = "err " & Err.Number
End Function

Public Function RetTypes(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: RetTypes = Empty
    Case 1: RetTypes = Null
    Case 2: RetTypes = CInt(5)
    Case 3: RetTypes = CLng(70000)
    Case 4: RetTypes = CSng(1.5)
    Case 5: RetTypes = CDbl(2.25)
    Case 6: RetTypes = CCur(3.5)
    Case 7: RetTypes = CDate(36526)
    Case 8: RetTypes = "str"
    Case 9: RetTypes = True
    Case 10: RetTypes = CVErr(2042)
    Case 11: RetTypes = CDec(1.25)
    Case 12: RetTypes = CByte(7)
    Case 13: RetTypes = Array(1, "a", Empty)
    Case 14: Set RetTypes = ThisWorkbook
    End Select
    Exit Function
EH:
    RetTypes = "err " & Err.Number
End Function
