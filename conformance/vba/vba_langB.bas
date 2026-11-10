Attribute VB_Name = "VbaLangB"
Option Explicit
Option Base 1
Option Compare Text

Private Function fI(x As Variant) As Integer
    fI = x
End Function
Private Function fL(x As Variant) As Long
    fL = x
End Function
Private Function fD(x As Variant) As Double
    fD = x
End Function
Private Function fS(x As Variant) As Single
    fS = x
End Function
Private Function fC(x As Variant) As Currency
    fC = x
End Function
Private Function fB(x As Variant) As Byte
    fB = x
End Function
Private Function fT(x As Variant) As Boolean
    fT = x
End Function
Private Function fV(x As Variant) As Variant
    fV = x
End Function
Private Function Z() As Long
    Z = 0
End Function

Public Function LB(ByVal k As Long) As Variant
    Select Case k \ 120
    Case 0: LB = LBC0(k)
    End Select
End Function

Private Function LBC0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: LBC0 = LBound(Array(1, 2))
    Case 1: LBC0 = UBound(Array(1, 2))
    Case 2: LBC0 = LBound(Split("a b"))
    Case 3: LBC0 = StrComp("a", "A")
    Case 4: LBC0 = "a" = "A"
    Case 5: LBC0 = "a" < "B"
    Case 6: LBC0 = InStr("abc", "B")
    Case 7: LBC0 = InStr(1, "abc", "B", vbBinaryCompare)
    Case 8: LBC0 = InStrRev("abc", "B")
    Case 9: LBC0 = "abc" Like "A*"
    Case 10: LBC0 = "abc" Like "[A-C]*"
    Case 11: LBC0 = Replace("abc", "B", "x")
    Case 12: LBC0 = Join(Filter(Array("ab", "AB"), "ab"), "|")
    Case 13: LBC0 = Join(Split("aXb", "x"), "|")
    Case 14: LBC0 = UCase("a") = "A"
    Case 15: LBC0 = fV("a") = fV("A")
    Case 16: LBC0 = fV("B") > fV("a")
    Case 17: LBC0 = "" & ChrW(233) & "" = "E"
    Case 18: LBC0 = "" & ChrW(452) & "" = "" & ChrW(454) & ""
    Case 19: LBC0 = StrComp("" & ChrW(233) & "", "E")
    Case 20: LBC0 = StrComp("a", "B")
    Case 21: LBC0 = StrComp("B", "a")
    Case 22: LBC0 = "a" < "B" And "B" < "c"
    Case 23: LBC0 = "A" < "b"
    Case 24: LBC0 = "a" > "B"
    Case 25: LBC0 = "" = ""
    Case 26: LBC0 = "a" <> "A"
    Case 27: LBC0 = InStr("" & ChrW(192) & "B", "" & ChrW(224) & "b")
    Case 28: LBC0 = StrComp("a", "A", vbBinaryCompare)
    Case 29: LBC0 = StrComp("a", "A", 2)
    Case 30: LBC0 = StrComp("a", "A", 0)
    Case 31: LBC0 = "" & ChrW(223) & "" = "ss"
    Case 32: LBC0 = "" & ChrW(223) & "" = "SS"
    Case 33: LBC0 = "" & ChrW(223) & "" < "ss"
    Case 34: LBC0 = "" & ChrW(223) & "" > "s"
    Case 35: LBC0 = "" & ChrW(223) & "" < "t"
    Case 36: LBC0 = "" & ChrW(230) & "" = "ae"
    Case 37: LBC0 = "" & ChrW(198) & "" = "ae"
    Case 38: LBC0 = "" & ChrW(339) & "" = "oe"
    Case 39: LBC0 = "" & ChrW(454) & "" = "d" & ChrW(382) & ""
    Case 40: LBC0 = "" & ChrW(454) & "" = "dz"
    Case 41: LBC0 = "" & ChrW(223) & "" Like "ss"
    Case 42: LBC0 = InStr("x" & ChrW(223) & "y", "ss")
    Case 43: LBC0 = StrComp("" & ChrW(223) & "a", "ssb")
    Case 44: LBC0 = fV("" & ChrW(223) & "") = fV("ss")
    Case 45: LBC0 = Replace("a" & ChrW(223) & "b", "ss", "-")
    Case 46: LBC0 = "ss" & ChrW(223) & "" = "" & ChrW(223) & "ss"
    Case 47: LBC0 = "s" = "" & ChrW(223) & ""
    Case 48: LBC0 = Join(Filter(Array("" & ChrW(223) & "", "ss", "s"), "ss"), "|")
    Case 49: LBC0 = LBP49()
    Case 50: LBC0 = LBP50()
    Case 51: LBC0 = LBP51()
    Case 52: LBC0 = LBP52()
    End Select
    Exit Function
EH:
    LBC0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LBP49() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(3) As Long, b(2, 2) As Long, c(0 To 2) As Long, s As String
    s = LBound(a) & UBound(a) & LBound(b, 1) & UBound(b, 2) & LBound(c)
    Dim d() As Long
    ReDim d(4)
    s = s & "|" & LBound(d) & UBound(d)
    Dim e() As Long
    ReDim e(2, 3)
    s = s & LBound(e, 2) & UBound(e, 2)
    s = s & "|" & LBound(Array(5, 6)) & UBound(Array())
    Dim v As Variant
    v = Array(7, 8, 9)
    s = s & "|" & v(1) & v(3)
    For Each v In Array(1, 2)
        s = s & v
    Next v
    R = s
    LBP49 = R
    Exit Function
EH:
    LBP49 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LBP50() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, a(1 To 3) As Long
    For i = LBound(a) To UBound(a)
        a(i) = i
    Next i
    Dim p As Variant
    p = Array("x", "y")
    s = p(1) & p(2) & UBound(p)
    On Error Resume Next
    s = s & p(0) & Err.Number
    R = s
    LBP50 = R
    Exit Function
EH:
    LBP50 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LBP51() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    Select Case "abc"
        Case "ABC": s = s & "ci"
        Case Else: s = s & "cs"
    End Select
    Select Case "b"
        Case "A" To "C": s = s & "r"
    End Select
    Select Case "b"
        Case Is > "A": s = s & "g"
    End Select
    Select Case "" & ChrW(233) & ""
        Case "E": s = s & "e"
        Case Else: s = s & "ne"
    End Select
    R = s
    LBP51 = R
    Exit Function
EH:
    LBP51 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LBP52() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, s As String
    v = "abc"
    s = (v = "ABC") & (v < "ABD") & (v > "ABB") & (v = 0) & (v > 5)
    Dim w As Variant
    w = "10"
    s = s & (w < "9") & (w < 9) & (w = 10)
    R = s
    LBP52 = R
    Exit Function
EH:
    LBP52 = "#ERR " & Err.Number & " " & Err.Description
End Function
