Attribute VB_Name = "VbaRtTextT"
Option Compare Text
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

Private Function RtTextT0(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: RtTextT0 = a0
    Case 1: RtTextT0 = InStr(a0, a1)
    Case 2: RtTextT0 = InStr(a0, a1, a2)
    Case 3: RtTextT0 = InStr(a0, a1, a2, a3)
    Case 4: RtTextT0 = InStrRev(a0, a1)
    Case 5: RtTextT0 = InStrRev(a0, a1, a2)
    Case 6: RtTextT0 = InStrRev(a0, a1, a2, a3)
    Case 7: RtTextT0 = InStrRev(a0, a1, , a3)
    Case 8: RtTextT0 = Replace(a0, a1, a2)
    Case 9: RtTextT0 = Replace(a0, a1, a2, a3)
    Case 10: RtTextT0 = Replace(a0, a1, a2, a3, a4)
    Case 11: RtTextT0 = Replace(a0, a1, a2, a3, a4, a5)
    Case 12: RtTextT0 = Replace(a0, a1, a2, , , a5)
    Case 13: RtTextT0 = Replace(a0, a1, a2, a3, , a5)
    Case 14: RtTextT0 = Replace(a0, a1, a2, , a4)
    Case 15: RtTextT0 = Replace(a0, a1, a2, , a4, a5)
    Case 16: RtTextT0 = StrComp(a0, a1)
    Case 17: RtTextT0 = StrComp(a0, a1, a2)
    Case 18: RtTextT0 = Dump(Split(a0, a1))
    Case 19: RtTextT0 = Dump(Split(a0))
    Case 20: RtTextT0 = Dump(Split(a0, a1, a2))
    Case 21: RtTextT0 = Dump(Split(a0, a1, a2, a3))
    Case 22: RtTextT0 = Dump(Split(a0, a1, , a3))
    Case 23: RtTextT0 = Join(Split(a0, a1), a2)
    Case 24: RtTextT0 = Join(Split(a0, a1))
    Case 25: RtTextT0 = Join(Array(a0, a1, a2), a3)
    Case 26: RtTextT0 = Join(Array(a0, a1), a2)
    Case 27: RtTextT0 = Join(Array(), a0)
    Case 28: RtTextT0 = Left(a0, a1)
    Case 29: RtTextT0 = Right(a0, a1)
    Case 30: RtTextT0 = Left$(a0, a1)
    Case 31: RtTextT0 = Right$(a0, a1)
    Case 32: RtTextT0 = Mid(a0, a1)
    Case 33: RtTextT0 = Mid(a0, a1, a2)
    Case 34: RtTextT0 = Mid$(a0, a1)
    Case 35: RtTextT0 = Mid$(a0, a1, a2)
    Case 36: RtTextT0 = Len(a0)
    Case 37: RtTextT0 = UCase(a0)
    Case 38: RtTextT0 = LCase(a0)
    Case 39: RtTextT0 = UCase$(a0)
    End Select
    Exit Function
EH:
    RtTextT0 = "#ERR " & Err.Number
End Function

Private Function RtTextT1(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    On Error GoTo EH
    Select Case k
    Case 40: RtTextT1 = LCase$(a0)
    Case 41: RtTextT1 = Trim(a0)
    Case 42: RtTextT1 = LTrim(a0)
    Case 43: RtTextT1 = RTrim(a0)
    Case 44: RtTextT1 = Trim$(a0)
    Case 45: RtTextT1 = LTrim$(a0)
    Case 46: RtTextT1 = RTrim$(a0)
    Case 47: RtTextT1 = Space(a0)
    Case 48: RtTextT1 = Space$(a0)
    Case 49: RtTextT1 = String(a0, a1)
    Case 50: RtTextT1 = String$(a0, a1)
    Case 51: RtTextT1 = StrReverse(a0)
    Case 52: RtTextT1 = Asc(a0)
    Case 53: RtTextT1 = AscW(a0)
    Case 54: RtTextT1 = Chr(a0)
    Case 55: RtTextT1 = Chr$(a0)
    Case 56: RtTextT1 = ChrW(a0)
    Case 57: RtTextT1 = ChrW$(a0)
    End Select
    Exit Function
EH:
    RtTextT1 = "#ERR " & Err.Number
End Function

Public Function RtTextT(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    Select Case k \ 40
    Case 0: RtTextT = RtTextT0(k, a0, a1, a2, a3, a4, a5)
    Case 1: RtTextT = RtTextT1(k, a0, a1, a2, a3, a4, a5)
    End Select
End Function
