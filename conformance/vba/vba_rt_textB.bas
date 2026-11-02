Attribute VB_Name = "VbaRtTextB"
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

Private Function RtTextB0(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: RtTextB0 = a0
    Case 1: RtTextB0 = InStr(a0, a1)
    Case 2: RtTextB0 = InStr(a0, a1, a2)
    Case 3: RtTextB0 = InStr(a0, a1, a2, a3)
    Case 4: RtTextB0 = InStrRev(a0, a1)
    Case 5: RtTextB0 = InStrRev(a0, a1, a2)
    Case 6: RtTextB0 = InStrRev(a0, a1, a2, a3)
    Case 7: RtTextB0 = InStrRev(a0, a1, , a3)
    Case 8: RtTextB0 = Replace(a0, a1, a2)
    Case 9: RtTextB0 = Replace(a0, a1, a2, a3)
    Case 10: RtTextB0 = Replace(a0, a1, a2, a3, a4)
    Case 11: RtTextB0 = Replace(a0, a1, a2, a3, a4, a5)
    Case 12: RtTextB0 = Replace(a0, a1, a2, , , a5)
    Case 13: RtTextB0 = Replace(a0, a1, a2, a3, , a5)
    Case 14: RtTextB0 = Replace(a0, a1, a2, , a4)
    Case 15: RtTextB0 = Replace(a0, a1, a2, , a4, a5)
    Case 16: RtTextB0 = StrComp(a0, a1)
    Case 17: RtTextB0 = StrComp(a0, a1, a2)
    Case 18: RtTextB0 = Dump(Split(a0, a1))
    Case 19: RtTextB0 = Dump(Split(a0))
    Case 20: RtTextB0 = Dump(Split(a0, a1, a2))
    Case 21: RtTextB0 = Dump(Split(a0, a1, a2, a3))
    Case 22: RtTextB0 = Dump(Split(a0, a1, , a3))
    Case 23: RtTextB0 = Join(Split(a0, a1), a2)
    Case 24: RtTextB0 = Join(Split(a0, a1))
    Case 25: RtTextB0 = Join(Array(a0, a1, a2), a3)
    Case 26: RtTextB0 = Join(Array(a0, a1), a2)
    Case 27: RtTextB0 = Join(Array(), a0)
    Case 28: RtTextB0 = Left(a0, a1)
    Case 29: RtTextB0 = Right(a0, a1)
    Case 30: RtTextB0 = Left$(a0, a1)
    Case 31: RtTextB0 = Right$(a0, a1)
    Case 32: RtTextB0 = Mid(a0, a1)
    Case 33: RtTextB0 = Mid(a0, a1, a2)
    Case 34: RtTextB0 = Mid$(a0, a1)
    Case 35: RtTextB0 = Mid$(a0, a1, a2)
    Case 36: RtTextB0 = Len(a0)
    Case 37: RtTextB0 = UCase(a0)
    Case 38: RtTextB0 = LCase(a0)
    Case 39: RtTextB0 = UCase$(a0)
    End Select
    Exit Function
EH:
    RtTextB0 = "#ERR " & Err.Number
End Function

Private Function RtTextB1(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    On Error GoTo EH
    Select Case k
    Case 40: RtTextB1 = LCase$(a0)
    Case 41: RtTextB1 = Trim(a0)
    Case 42: RtTextB1 = LTrim(a0)
    Case 43: RtTextB1 = RTrim(a0)
    Case 44: RtTextB1 = Trim$(a0)
    Case 45: RtTextB1 = LTrim$(a0)
    Case 46: RtTextB1 = RTrim$(a0)
    Case 47: RtTextB1 = Space(a0)
    Case 48: RtTextB1 = Space$(a0)
    Case 49: RtTextB1 = String(a0, a1)
    Case 50: RtTextB1 = String$(a0, a1)
    Case 51: RtTextB1 = StrReverse(a0)
    Case 52: RtTextB1 = Asc(a0)
    Case 53: RtTextB1 = AscW(a0)
    Case 54: RtTextB1 = Chr(a0)
    Case 55: RtTextB1 = Chr$(a0)
    Case 56: RtTextB1 = ChrW(a0)
    Case 57: RtTextB1 = ChrW$(a0)
    End Select
    Exit Function
EH:
    RtTextB1 = "#ERR " & Err.Number
End Function

Public Function RtTextB(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    Select Case k \ 40
    Case 0: RtTextB = RtTextB0(k, a0, a1, a2, a3, a4, a5)
    Case 1: RtTextB = RtTextB1(k, a0, a1, a2, a3, a4, a5)
    End Select
End Function
