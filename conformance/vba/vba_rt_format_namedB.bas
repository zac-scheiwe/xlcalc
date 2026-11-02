Attribute VB_Name = "VbaRtFmtB"
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

Private Function RtFmtB0(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: RtFmtB0 = Format(a0, a1)
    Case 1: RtFmtB0 = Format(a0, a1, a2)
    Case 2: RtFmtB0 = Format(a0)
    Case 3: RtFmtB0 = Format$(a0)
    Case 4: RtFmtB0 = Format$(a0, a1)
    Case 5: RtFmtB0 = Format(a0, a1, a2, a3)
    Case 6: RtFmtB0 = Format(a0, a1, , a3)
    End Select
    Exit Function
EH:
    RtFmtB0 = "#ERR " & Err.Number
End Function

Public Function RtFmtB(ByVal k As Long, ByVal a0 As Variant, ByVal a1 As Variant, ByVal a2 As Variant, ByVal a3 As Variant, ByVal a4 As Variant, ByVal a5 As Variant) As Variant
    Select Case k \ 40
    Case 0: RtFmtB = RtFmtB0(k, a0, a1, a2, a3, a4, a5)
    End Select
End Function
