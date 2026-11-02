Attribute VB_Name = "VbaRtConvB"
Option Explicit

Private Function Dump(v As Variant) As String
    Dim i As Long, s As String
    s = "A" & LBound(v) & ":" & UBound(v) & ":"
    For i = LBound(v) To UBound(v)
        If i > LBound(v) Then s = s & "|"
        s = s & v(i)
    Next i
    Dump = s
End Function

Private Function RtConvB0(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: RtConvB0 = CInt(a0)
    Case 1: RtConvB0 = CLng(a0)
    Case 2: RtConvB0 = CByte(a0)
    Case 3: RtConvB0 = CBool(a0)
    Case 4: RtConvB0 = CCur(a0)
    Case 5: RtConvB0 = CDbl(a0)
    Case 6: RtConvB0 = CSng(a0)
    Case 7: RtConvB0 = CDec(a0)
    Case 8: RtConvB0 = CStr(a0)
    Case 9: RtConvB0 = Int(a0)
    Case 10: RtConvB0 = Fix(a0)
    Case 11: RtConvB0 = Abs(a0)
    Case 12: RtConvB0 = Sgn(a0)
    Case 13: RtConvB0 = CVar(a0)
    Case 14: RtConvB0 = TypeName(a0)
    Case 15: RtConvB0 = VarType(a0)
    Case 16: RtConvB0 = IsNumeric(a0)
    Case 17: RtConvB0 = IsNull(a0)
    Case 18: RtConvB0 = IsEmpty(a0)
    Case 19: RtConvB0 = IsError(a0)
    Case 20: RtConvB0 = IsArray(a0)
    Case 21: RtConvB0 = IIf(a0, a1, a2)
    Case 22: RtConvB0 = Val(a0)
    Case 23: RtConvB0 = Str(a0)
    Case 24: RtConvB0 = Str$(a0)
    Case 25: RtConvB0 = Hex(a0)
    Case 26: RtConvB0 = Oct(a0)
    Case 27: RtConvB0 = Hex$(a0)
    Case 28: RtConvB0 = Oct$(a0)
    Case 29: RtConvB0 = Round(a0)
    Case 30: RtConvB0 = Round(a0, a1)
    Case 31: RtConvB0 = Sqr(a0)
    Case 32: RtConvB0 = Log(a0)
    Case 33: RtConvB0 = Exp(a0)
    Case 34: RtConvB0 = Atn(a0)
    Case 35: RtConvB0 = Sin(a0)
    Case 36: RtConvB0 = Cos(a0)
    Case 37: RtConvB0 = Tan(a0)
    Case 38: RtConvB0 = CVErr(a0)
    End Select
    Exit Function
EH:
    RtConvB0 = "#ERR " & Err.Number
End Function

Public Function RtConvB(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    Select Case k \ 40
    Case 0: RtConvB = RtConvB0(k, a0, a1, a2, a3, a4, a5)
    End Select
End Function
