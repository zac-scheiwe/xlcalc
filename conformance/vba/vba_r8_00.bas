Attribute VB_Name = "VbaR8_00"
Option Explicit
Public R8Log As String
Private Type R8P
    X As Long
    Y As Double
    S As String
    B(1 To 3) As Byte
End Type
Private Type R8Q
    P As R8P
    F As String * 4
    Tag As Integer
    C As Currency
    D As Date
    G As Single
End Type
Private Type R8Odd
    A As Byte
    B As Long
    C As Integer
    D As Double
    E As Boolean
End Type
Private Enum R8E
    ea = 1
    eb = 5
    ec
    ed = -2
    [_Hide] = 40
    ee = &H10
End Enum
Private Enum R8F
    f1 = 1
    f2 = 2
    f4 = 4
    f8 = 8
End Enum
#Const R8A = 3
#Const R8B = "s"
#Const R8C = True
#Const R8D = R8A * 2 + 1
#Const R8N = 0
Private Static_n As Long
Private Function fV(x As Variant) As Variant
    fV = x
End Function
Private Function Hx(ByVal s As String) As String
    Dim i As Long, t As String, bad As Boolean
    For i = 1 To Len(s)
        If AscW(Mid$(s, i, 1)) < 32 Or AscW(Mid$(s, i, 1)) > 126 Then bad = True
    Next i
    If Not bad Then
        Hx = "'" & s & "'"
    Else
        For i = 1 To Len(s)
            t = t & Hex$(AscW(Mid$(s, i, 1)) And &HFFFF&) & " "
        Next i
        Hx = "<" & t & ">"
    End If
End Function
Private Function Dmp(v As Variant) As String
    Dim s As String, i As Long
    If IsArray(v) Then
        On Error Resume Next
        s = "A" & LBound(v) & ":" & UBound(v) & "["
        If Err.Number <> 0 Then Dmp = "A<" & Err.Number & ">": Exit Function
        On Error GoTo 0
        For i = LBound(v) To UBound(v)
            s = s & Dmp(v(i)) & ","
        Next i
        Dmp = s & "]"
    ElseIf IsNull(v) Then
        Dmp = "Null"
    ElseIf IsEmpty(v) Then
        Dmp = "Empty"
    ElseIf IsObject(v) Then
        Dmp = "<" & TypeName(v) & ">"
    ElseIf VarType(v) = vbDouble Or VarType(v) = vbSingle Then
        Dmp = TypeName(v) & ":" & CStr(v) & "~" & CStr(v - CDbl(CStr(v)))
    Else
        Dmp = TypeName(v) & ":" & Hx(CStr(v))
    End If
End Function
Private Sub ByRefL(ByRef n As Long)
    n = n + 1
End Sub
Private Sub Alias2(ByRef a As Long, ByRef b As Long)
    a = a + 1
    b = b * 2
End Sub
Private Sub ByValL(ByVal n As Long)
    n = n + 1
End Sub
Private Sub ByRefV(ByRef v As Variant)
    v = v & "x"
End Sub
Private Function Opt1(Optional a As Variant, Optional b As Long = 7, Optional c As String = "d") As String
    Opt1 = IIf(IsMissing(a), "M", "a=" & TypeName(a) & a) & "|" & b & "|" & c
End Function
Private Function Opt2(Optional a As Long, Optional b As Variant = Empty, Optional ByVal e As R8E = eb) As String
    Opt2 = a & "|" & TypeName(b) & "|" & e
End Function
Private Function OptFwd(Optional a As Variant) As String
    OptFwd = Opt1(a)
End Function
Private Function PA(ParamArray p() As Variant) As String
    Dim i As Long, s As String
    s = LBound(p) & ":" & UBound(p) & ":"
    For i = LBound(p) To UBound(p)
        s = s & TypeName(p(i)) & ","
    Next i
    PA = s
End Function
Private Function PAfwd(ParamArray p() As Variant) As String
    PAfwd = PA(p)
End Function
Private Sub PAmod(ParamArray p() As Variant)
    p(0) = p(0) + 1
End Sub
Private Function PA2(ByVal a As Long, ParamArray p() As Variant) As String
    PA2 = a & ":" & (UBound(p) - LBound(p) + 1)
End Function
Private Function StatCnt() As Long
    Static n As Long
    n = n + 1
    StatCnt = n
End Function
Private Static Function StatAll() As String
    Dim q As Long, z As String
    q = q + 1
    z = z & "x"
    StatAll = q & z
End Function
Private Function StatArr(ByVal i As Long) As Long
    Static a(1 To 3) As Long
    a(i) = a(i) + i
    StatArr = a(1) * 100 + a(2) * 10 + a(3)
End Function
Private Function Fact(ByVal n As Long) As Double
    If n <= 1 Then Fact = 1 Else Fact = n * Fact(n - 1)
End Function
Private Function NoRet() As Long
    Exit Function
End Function
Private Function RetArr(ByVal n As Long) As Long()
    Dim a() As Long, i As Long
    ReDim a(1 To n)
    For i = 1 To n
        a(i) = i * i
    Next i
    RetArr = a
End Function
Private Function Lg(ByVal s As String, ByVal r As Boolean) As Boolean
    R8Log = R8Log & s
    Lg = r
End Function
Private Sub Thrower(ByVal n As Long)
    Err.Raise n
End Sub
Private Sub ThrowerQuiet(ByVal n As Long)
    On Error Resume Next
    Err.Raise n
End Sub
Private Sub Fixer(ByRef d As Variant)
    d = 1
End Sub
Private Function GsF(ByVal n As Long) As String
    Dim s As String
    s = "a"
    GoSub Sub1
    s = s & "b"
    If n = 1 Then GoSub Sub2
    GsF = s
    Exit Function
Sub1:
    s = s & "1"
    Return
Sub2:
    s = s & "2"
    GoSub Sub1
    Return
End Function
#If R8A > 2 Then
Private Function CcF() As String
    CcF = "big"
End Function
#Else
Private Function CcF() As String
    CcF = "small"
End Function
#End If
Private Function ByRefLF(ByRef n As Long) As Long
    n = n + 1
    ByRefLF = n
End Function
Private Function Z0() As Long
    Z0 = 0
End Function
Public Function VbEight0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k \ 50
    Case 0: VbEight0 = MC0(k)
    Case 1: VbEight0 = MC1(k)
    Case 2: VbEight0 = MC2(k)
    End Select
    Exit Function
EH:
    VbEight0 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function MC0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: MC0 = M0()
    Case 1: MC0 = M1()
    Case 2: MC0 = M2()
    Case 3: MC0 = M3()
    Case 4: MC0 = M4()
    Case 5: MC0 = M5()
    Case 6: MC0 = M6()
    Case 7: MC0 = M7()
    Case 8: MC0 = M8()
    Case 9: MC0 = M9()
    Case 10: MC0 = M10()
    Case 11: MC0 = M11()
    Case 12: MC0 = M12()
    Case 13: MC0 = M13()
    Case 14: MC0 = M14()
    Case 15: MC0 = M15()
    Case 16: MC0 = M16()
    Case 17: MC0 = M17()
    Case 18: MC0 = M18()
    Case 19: MC0 = M19()
    Case 20: MC0 = M20()
    Case 21: MC0 = M21()
    Case 22: MC0 = M22()
    Case 23: MC0 = M23()
    Case 24: MC0 = M24()
    Case 25: MC0 = M25()
    Case 26: MC0 = M26()
    Case 27: MC0 = M27()
    Case 28: MC0 = M28()
    Case 29: MC0 = M29()
    Case 30: MC0 = M30()
    Case 31: MC0 = M31()
    Case 32: MC0 = M32()
    Case 33: MC0 = M33()
    Case 34: MC0 = M34()
    Case 35: MC0 = M35()
    Case 36: MC0 = M36()
    Case 37: MC0 = M37()
    Case 38: MC0 = M38()
    Case 39: MC0 = M39()
    Case 40: MC0 = M40()
    Case 41: MC0 = M41()
    Case 42: MC0 = M42()
    Case 43: MC0 = M43()
    Case 44: MC0 = M44()
    Case 45: MC0 = M45()
    Case 46: MC0 = M46()
    Case 47: MC0 = M47()
    Case 48: MC0 = M48()
    Case 49: MC0 = M49()
    End Select
    Exit Function
EH:
    MC0 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function MC1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 50: MC1 = M50()
    Case 51: MC1 = M51()
    Case 52: MC1 = M52()
    Case 53: MC1 = M53()
    Case 54: MC1 = M54()
    Case 55: MC1 = M55()
    Case 56: MC1 = M56()
    Case 57: MC1 = M57()
    Case 58: MC1 = M58()
    Case 59: MC1 = M59()
    Case 60: MC1 = M60()
    Case 61: MC1 = M61()
    Case 62: MC1 = M62()
    Case 63: MC1 = M63()
    Case 64: MC1 = M64()
    Case 65: MC1 = M65()
    Case 66: MC1 = M66()
    Case 67: MC1 = M67()
    Case 68: MC1 = M68()
    Case 69: MC1 = M69()
    Case 70: MC1 = M70()
    Case 71: MC1 = M71()
    Case 72: MC1 = M72()
    Case 73: MC1 = M73()
    Case 74: MC1 = M74()
    Case 75: MC1 = M75()
    Case 76: MC1 = M76()
    Case 77: MC1 = M77()
    Case 78: MC1 = M78()
    Case 79: MC1 = M79()
    Case 80: MC1 = M80()
    Case 81: MC1 = M81()
    Case 82: MC1 = M82()
    Case 83: MC1 = M83()
    Case 84: MC1 = M84()
    Case 85: MC1 = M85()
    Case 86: MC1 = M86()
    Case 87: MC1 = M87()
    Case 88: MC1 = M88()
    Case 89: MC1 = M89()
    Case 90: MC1 = M90()
    Case 91: MC1 = M91()
    Case 92: MC1 = M92()
    Case 93: MC1 = M93()
    Case 94: MC1 = M94()
    Case 95: MC1 = M95()
    Case 96: MC1 = M96()
    Case 97: MC1 = M97()
    Case 98: MC1 = M98()
    Case 99: MC1 = M99()
    End Select
    Exit Function
EH:
    MC1 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function MC2(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 100: MC2 = M100()
    Case 101: MC2 = M101()
    Case 102: MC2 = M102()
    Case 103: MC2 = M103()
    Case 104: MC2 = M104()
    Case 105: MC2 = M105()
    Case 106: MC2 = M106()
    Case 107: MC2 = M107()
    Case 108: MC2 = M108()
    Case 109: MC2 = M109()
    Case 110: MC2 = M110()
    Case 111: MC2 = M111()
    Case 112: MC2 = M112()
    Case 113: MC2 = M113()
    Case 114: MC2 = M114()
    Case 115: MC2 = M115()
    Case 116: MC2 = M116()
    Case 117: MC2 = M117()
    Case 118: MC2 = M118()
    Case 119: MC2 = M119()
    Case 120: MC2 = M120()
    Case 121: MC2 = M121()
    Case 122: MC2 = M122()
    Case 123: MC2 = M123()
    Case 124: MC2 = M124()
    Case 125: MC2 = M125()
    Case 126: MC2 = M126()
    Case 127: MC2 = M127()
    Case 128: MC2 = M128()
    Case 129: MC2 = M129()
    Case 130: MC2 = M130()
    Case 131: MC2 = M131()
    Case 132: MC2 = M132()
    Case 133: MC2 = M133()
    Case 134: MC2 = M134()
    Case 135: MC2 = M135()
    Case 136: MC2 = M136()
    Case 137: MC2 = M137()
    Case 138: MC2 = M138()
    Case 139: MC2 = M139()
    Case 140: MC2 = M140()
    Case 141: MC2 = M141()
    Case 142: MC2 = M142()
    Case 143: MC2 = M143()
    Case 144: MC2 = M144()
    Case 145: MC2 = M145()
    Case 146: MC2 = M146()
    Case 147: MC2 = M147()
    Case 148: MC2 = M148()
    Case 149: MC2 = M149()
    End Select
    Exit Function
EH:
    MC2 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M0() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, x As Variant, i As Long, n As Long
10  On Error GoTo H
20  s = "s"
30  n = n + 1
40  x = 1 / i
50  s = s & "ok" & n
60  R = s
70  Exit Function
H:
    s = s & "[" & Err.Number & " erl " & Erl & "]"
    n = n + 1
    i = 1 + n
    If n < 6 Then Resume
    R = s & "gave up"
    M0 = R
    Exit Function
EH:
    M0 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M1() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long, i As Long, s As String
    ReDim a(1 To 2)
    For i = LBound(a) To UBound(a)
        a(i) = i * 3
    Next i
    ReDim Preserve a(0 To 1)
    For i = LBound(a) To UBound(a)
        s = s & a(i) & ","
    Next i
    R = s & LBound(a) & ":" & UBound(a) & ":" & Dmp(a)
    M1 = R
    Exit Function
EH:
    M1 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M2() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = TypeName(CInt("Hello World")) & ":" & Dmp(CInt("Hello World"))
    M2 = R
    Exit Function
EH:
    M2 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M3() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R8Sink
    Set s = New R8Sink
    R8Log = ""
    s.Id = "q"
    s.Src.Fire
    R = R8Log
    M3 = R
    Exit Function
EH:
    M3 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M4() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Replace("abcabc", "abc", "-", -1, 2, 5)) & "|" & Dmp(Replace("abcabc", "abc", "-"))
    M4 = R
    Exit Function
EH:
    M4 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M5() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = CVErr(-1)
    R = Dmp(v) & TypeName(v) & VarType(v) & IsError(v) & Dmp(v = v) & Dmp(v & "")
    M5 = R
    Exit Function
EH:
    M5 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M6() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV("12:30")
    R = TypeName(CLng(v)) & ":" & Dmp(CLng(v)) & "|" & VarType(v) & IsNumeric(v) & IsDate(v) & IsEmpty(v) & IsNull(v)
    M6 = R
    Exit Function
EH:
    M6 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M7() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim e As R8F, v As Variant
    e = fV(ee)
    R = Dmp(e) & "|" & Dmp(e + 1) & "|" & Dmp(CStr(e)) & "|" & TypeName(e) & "|" & Dmp(e * 2) & "|" & Dmp(e And 3) & "|" & Format(e, "00") & "|" & VarType(e) & Dmp(e / 2) & Dmp(-e) & Dmp(Not e)
    M7 = R
    Exit Function
EH:
    M7 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M8() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, b As Variant, c As Variant
    a = CDec(fV(5)): b = CDec(fV(1E15)): c = CDec(fV(-1))
    R = Dmp(a Or b) & "|" & Dmp((a / b) / c) & "|" & Dmp(a / 1E10) & "|" & Dmp(a / CCur(c))
    M8 = R
    Exit Function
EH:
    M8 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M9() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, c As New Collection, s As String, a(1 To 3) As Long, n As Long
    c.Add "x": c.Add 2: c.Add "z"
    a(1) = 5: a(2) = 6: a(3) = 7
    For Each v In c
        s = s & Dmp(v) & ","
        n = n + 1
        If n > 8 Then Exit For
    Next v
    R = s & Dmp(v)
    M9 = R
    Exit Function
EH:
    M9 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M10() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long
    s = Empty & Empty
    n = Empty & False
    R = Dmp(s) & Dmp(n) & Dmp(Empty & False) & Dmp(Empty + False) & Dmp(Empty & Null) & Dmp(Empty + Null)
    M10 = R
    Exit Function
EH:
    M10 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M11() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, j As Long, n As Long, s As String, lim As Long
    lim = 3
    For i = 1 To lim
        lim = 10
        i = i + 2
        s = s & i & ","
        n = n + 1
        If n > 15 Then Exit For
    Next i
    R = s & n & ":" & i & ":" & lim
    M11 = R
    Exit Function
EH:
    M11 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M12() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(3.14159265358979)
    R = Dmp(Round(v, 15)) & "|" & Dmp(Int(v)) & "|" & Dmp(Fix(v)) & "|" & Dmp(Abs(v)) & "|" & Dmp(Sgn(v)) & "|" & Dmp(CInt(v)) & "|" & Dmp(CLng(v))
    M12 = R
    Exit Function
EH:
    M12 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M13() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, c As Collection, s As String, n As Long
    Set c = New Collection
    c.Add 1: c.Add 2: c.Add 3
    For Each v In c
        s = s & v & ","
        n = n + 1
        If v = 3 Then c.Add 9
        If n > 8 Then Exit For
    Next v
    R = s & c.Count
    M13 = R
    Exit Function
EH:
    M13 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M14() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String, x As Variant, arr(1 To 2) As Long, o As Object
    On Error Resume Next
    For i = 1 To 3
        s = s & Err.Number & ","
        x = -2147483648# \ -1
        s = s & Err.Number & "/" & Left$(Err.Description, 30) & ";"
        x = DateSerial(10000, 1, 1)
        s = s & Err.Number & ";"
        Err.Clear
    Next i
    R = s
    M14 = R
    Exit Function
EH:
    M14 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M15() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Integer, v As Variant, n As Long
    On Error Resume Next
    n = UBound(a)
    R = n & ":" & Err.Number & ":" & LBound(a) & ":" & Err.Number & ":" & IsArray(a) & ":" & IsArray(v)
    v = a
    R = R & ":" & IsArray(v) & ":" & TypeName(v) & ":" & TypeName(a) & ":" & VarType(a)
    ReDim a(0)
    R = R & ":" & UBound(a) & ":" & TypeName(a) & ":" & VarType(a) & ":" & Len(a(0))
    M15 = R
    Exit Function
EH:
    M15 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M16() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Currency, d As Double, v As Variant
    c = fV(922337203685477.5807@): d = fV(10000): v = c * d
    R = Dmp(c) & "|" & Dmp(v) & "|" & Dmp(c / 3) & "|" & Dmp(c * 3 / 3) & "|" & Dmp(c + d) & "|" & Dmp(c \ 7) & "|" & Dmp(c ^ 2)
    M16 = R
    Exit Function
EH:
    M16 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M17() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long, y As Variant, a(1 To 3) As Long, s As String
    x = 5: y = 5: a(1) = 5
    Call ByRefL(x + 0)
    R = x & "|" & Dmp(y) & "|" & a(1) & "|" & s
    M17 = R
    Exit Function
EH:
    M17 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M18() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(256)
    R = TypeName(v) & VarType(v) & IsNumeric(v) & IsDate(v) & IsEmpty(v) & IsNull(v) & IsError(v) & IsObject(v) & IsArray(v) & "|" & Dmp(CVErr(-1)) & "|" & Dmp(Hex(v)) & Dmp(Oct(v)) & Dmp(Str(v)) & Dmp(Val(v)) & Dmp(Len(v))
    M18 = R
    Exit Function
EH:
    M18 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M19() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(0.00005@)
    R = FormatNumber(v, 2, -2, -2, -2) & "|" & FormatCurrency(v, 2, -2, -2, -2) & "|" & FormatPercent(v, 2, -2, -2, -2)
    M19 = R
    Exit Function
EH:
    M19 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M20() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, n As Long, s As String
    i = 5
    Do While i < 8
        i = i + 1
        n = n + 1
        If n > 10 Then Exit Do
    Loop
    s = i & "," & n
    Do
        i = i - 1: n = n - 1
    Loop Until i < 5 Or n < -5
    While n < 3
        n = n + 2
    Wend
    R = s & "|" & i & "," & n
    M20 = R
    Exit Function
EH:
    M20 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M21() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, c As New Collection, s As String, a(1 To 3) As Long, n As Long
    c.Add "x": c.Add 2: c.Add "z"
    a(1) = 5: a(2) = 6: a(3) = 7
    For Each v In Worksheets
        s = s & Dmp(v) & ","
        n = n + 1
        If n > 8 Then Exit For
    Next v
    R = s & Dmp(v)
    M21 = R
    Exit Function
EH:
    M21 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M22() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(fV(Null))
    M22 = R
    Exit Function
EH:
    M22 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M23() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long, s As String
    x = 0
    If x > 3 Then s = "gt": If x > 6 Then s = s & "6"
    If x = 1 Then s = s & "one" Else If x = 2 Then s = s & "two" Else s = s & "oth"
    If x > 0 Then
        s = s & "p"
    ElseIf x = 0 Then
        s = s & "z"
    Else
        s = s & "n"
    End If
    R = s
    M23 = R
    Exit Function
EH:
    M23 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M24() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(CDec("1234.5678"), "0.0;-0.0;""zero""") & "|" & Format(CDec("1234.5678"), """x""0") & "|" & Format(CDec("1234.5678"), "0.0;-0.0;""zero"";""x""0")
    M24 = R
    Exit Function
EH:
    M24 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M25() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = TypeName(1 + 10) & ":" & Dmp(1 + 10)
    M25 = R
    Exit Function
EH:
    M25 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M26() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3, 1 To 2) As Long, i As Long, j As Long
    For i = 1 To 3: For j = 1 To 2: a(i, j) = i * j: Next j: Next i
    R = UBound(a, 1) & UBound(a, 2) & ":" & a(3, 2) & ":" & a(4, 1)
    M26 = R
    Exit Function
EH:
    M26 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M27() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3, 1 To 2) As Long, i As Long, j As Long
    For i = 1 To 3: For j = 1 To 2: a(i, j) = i * j: Next j: Next i
    R = UBound(a, 1) & UBound(a, 2) & ":" & a(3, 2) & ":" & a(0, 3)
    M27 = R
    Exit Function
EH:
    M27 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M28() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, w As Variant
    v = Array(1, Array(2, 3), "x")
    w = v
    w(0) = 100
    R = Dmp(v) & Dmp(w) & v(1)(1) & IsArray(v(1)) & UBound(v(1)) & Dmp(v(0))
    M28 = R
    Exit Function
EH:
    M28 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M29() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Variant
    d = fV(CDate(36526.75))
    R = Format(d, "A/P") & "|" & Format(d, "Long Time") & "|" & FormatDateTime(d, 5)
    M29 = R
    Exit Function
EH:
    M29 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M30() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "0.00,,") & "|" & Format(1234.5678, "#,##0;;") & "|" & Format(1234.5678, "0.00,,;#,##0;;")
    M30 = R
    Exit Function
EH:
    M30 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M31() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R8Sink
    Set s = New R8Sink
    R8Log = ""
    s.Id = "q"
    s.Src.Fire
    R = R8Log
    M31 = R
    Exit Function
EH:
    M31 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M32() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(True)
    R = FormatNumber(v, 2, -2, -1, -2) & "|" & FormatCurrency(v, 2, -2, -1, -2) & "|" & FormatPercent(v, 2, -2, -1, -2)
    M32 = R
    Exit Function
EH:
    M32 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M33() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, n As Long, s As String
    i = 1
    Do While i < 3
        i = i + 3
        n = n + 1
        If n > 10 Then Exit Do
    Loop
    s = i & "," & n
    Do
        i = i - 1: n = n - 1
    Loop Until i < 1 Or n < -5
    While n < 3
        n = n + 2
    Wend
    R = s & "|" & i & "," & n
    M33 = R
    Exit Function
EH:
    M33 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M34() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection, s As String, v As Variant
    On Error Resume Next
    With c
        .Add 1, "a"
        .Add 2, "a"
        s = s & Err.Number & ","
        .Add 3
        s = s & .Item(4) & "," & Err.Number & ","
        s = s & .Item("zz") & "," & Err.Number & ","
        .Remove 4
        s = s & .Count & "," & Err.Number
    End With
    R = s
    M34 = R
    Exit Function
EH:
    M34 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M35() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Date, d2 As Date
    d = fV(#11:59:59 PM#): d2 = fV(#12/31/9999#)
    R = Dmp(DateAdd("yyyy", 2147483647, d)) & "|" & Dmp(DateDiff("yyyy", d, d2, 8, 4)) & "|" & Dmp(DatePart("yyyy", d, 8, 4))
    M35 = R
    Exit Function
EH:
    M35 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M36() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 1, t As String, u As String * 3
    s = 1.5
    t = s & "|"
    u = "a"
    R = Hx(s) & Len(s) & LenB(s) & Hx(t) & Hx(u) & Hx(u & "x") & Hx(RTrim(u)) & (u = "a") & (u = "a  ")
    Mid(s, 2, 2) = "QQQ"
    R = R & Hx(s)
    LSet s = "k"
    R = R & Hx(s)
    RSet s = "k"
    R = R & Hx(s)
    M36 = R
    Exit Function
EH:
    M36 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M37() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Switch(0, 1, False, 2)) & Dmp(Choose(0, "a", "b", "c")) & Dmp(IIf(0, "t", "f")) & Dmp(Partition(100, 0, 99, 10)) & Dmp(Partition(100, 1, 20, 5))
    M37 = R
    Exit Function
EH:
    M37 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M38() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b As R8Box, o As Object
    Set b = New R8Box
    R8Log = ""
    CallByName b, "N", VbLet, 6: R = b.N
    R = R & "|" & R8Log
    R8Log = ""
    M38 = R
    Exit Function
EH:
    M38 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M39() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String, x As Variant, arr(1 To 2) As Long, o As Object
    On Error Resume Next
    For i = 1 To 4
        s = s & Err.Number & ","
        x = InStr(0, "a", "b")
        s = s & Err.Number & "/" & Left$(Err.Description, 30) & ";"
        x = Split("a")(i + 3)
        s = s & Err.Number & ";"
        Err.Clear
    Next i
    R = s
    M39 = R
    Exit Function
EH:
    M39 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M40() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String
    s = " a b ": t = "l"
    R = InStr(s, t) & "|" & InStr(100, s, t, 5) & "|" & InStrRev(s, t) & "|" & InStrRev(s, t, 100, 5) & "|" & StrComp(s, t, 5) & "|" & StrComp(s, t)
    M40 = R
    Exit Function
EH:
    M40 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M41() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Integer, v As Variant, n As Long
    On Error Resume Next
    n = UBound(a)
    R = n & ":" & Err.Number & ":" & LBound(a) & ":" & Err.Number & ":" & IsArray(a) & ":" & IsArray(v)
    v = a
    R = R & ":" & IsArray(v) & ":" & TypeName(v) & ":" & TypeName(a) & ":" & VarType(a)
    ReDim a(0)
    R = R & ":" & UBound(a) & ":" & TypeName(a) & ":" & VarType(a) & ":" & Len(a(0))
    M41 = R
    Exit Function
EH:
    M41 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M42() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(1E-5)
    R = Format(v, "0.00,,") & "|" & Format(v, "\x0") & "|" & Format$(v)
    M42 = R
    Exit Function
EH:
    M42 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M43() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long, i As Long, s As String
    ReDim a(2 To 3)
    For i = LBound(a) To UBound(a)
        a(i) = i * 3
    Next i
    ReDim Preserve a(-1 To 6)
    For i = LBound(a) To UBound(a)
        s = s & a(i) & ","
    Next i
    R = s & LBound(a) & ":" & UBound(a) & ":" & Dmp(a)
    M43 = R
    Exit Function
EH:
    M43 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M44() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "&H1F"
    R = Dmp(Left(s, -1)) & Dmp(Right(s, -1)) & Dmp(Mid(s, -1)) & Dmp(Mid(s, -1, 100)) & Dmp(Trim(s)) & Dmp(LTrim(s)) & Dmp(RTrim(s)) & Dmp(UCase(s)) & Dmp(LCase(s)) & Dmp(StrReverse(s)) & Len(s) & "," & LenB(s)
    M44 = R
    Exit Function
EH:
    M44 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M45() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b() As Byte, s As String, i As Long, t As String
    s = "a" & ChrW(233)
    b = s
    For i = LBound(b) To UBound(b)
        t = t & b(i) & ","
    Next i
    b = StrConv(s, vbFromUnicode)
    t = t & "|" & UBound(b) & "|"
    s = b
    R = t & Hx(s) & "|" & Hx(StrConv(b, vbUnicode)) & "|" & Hx(CStr(StrConv(s, vbFromUnicode)))
    M45 = R
    Exit Function
EH:
    M45 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M46() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV("abc" & ChrW(233) & "xyz")
    R = TypeName(v) & VarType(v) & IsNumeric(v) & IsDate(v) & IsEmpty(v) & IsNull(v) & IsError(v) & IsObject(v) & IsArray(v) & "|" & Dmp(CVErr(7)) & "|" & Dmp(Hex(v)) & Dmp(Oct(v)) & Dmp(Str(v)) & Dmp(Val(v)) & Dmp(Len(v))
    M46 = R
    Exit Function
EH:
    M46 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M47() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = 0
    R = Dmp(Hex(x)) & Dmp(Oct(x)) & Dmp(Hex(-x)) & Dmp(&H7FFF + x) & Dmp(&HFFFF) & Dmp(&HFFFF&) & Dmp(&H7FFFFFFF) & Dmp(&H80000000) & Dmp(&O17) & Dmp(CLng("&HFFFF")) & Dmp(Val("&HFFFF")) & Dmp(Val("&H7FFFFFFF&"))
    M47 = R
    Exit Function
EH:
    M47 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M48() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b As R8Box, s As R8Sink
    R8Log = ""
    Set b = New R8Box: b.Tag = "b1"
    Set s = New R8Sink: s.Id = "s"
    Set s.Src = b
    b.Fire
    Set s.Src = New R8Box
    s.Src.Tag = "b2"
    b.Fire: s.Src.Fire
    Set b = Nothing
    R = R8Log
    Set s = Nothing
    R = R & "|" & R8Log
    R8Log = ""
    M48 = R
    Exit Function
EH:
    M48 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M49() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Variant
    d = fV(CDate("2021-03-15"))
    R = Dmp(Year(d)) & Dmp(Month(d)) & Dmp(Day(d)) & Dmp(Hour(d)) & Dmp(Minute(d)) & Dmp(Second(d)) & Dmp(Weekday(d, 7)) & Dmp(DateValue(d)) & Dmp(TimeValue(d)) & Dmp(CDbl(d)) & Dmp(Int(d))
    M49 = R
    Exit Function
EH:
    M49 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M50() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, n As Long, s As String
    i = 0
    Do While i < 8
        i = i + 3
        n = n + 1
        If n > 10 Then Exit Do
    Loop
    s = i & "," & n
    Do
        i = i - 1: n = n - 1
    Loop Until i < 0 Or n < -5
    While n < 3
        n = n + 2
    Wend
    R = s & "|" & i & "," & n
    M50 = R
    Exit Function
EH:
    M50 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M51() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String
    s = "aBc": t = "l"
    R = InStr(s, t) & "|" & InStr(100, s, t, 5) & "|" & InStrRev(s, t) & "|" & InStrRev(s, t, 100, 5) & "|" & StrComp(s, t, 5) & "|" & StrComp(s, t)
    M51 = R
    Exit Function
EH:
    M51 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M52() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = TypeName(2147483648# \ 100) & ":" & Dmp(2147483648# \ 100)
    M52 = R
    Exit Function
EH:
    M52 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M53() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = TypeName(CInt("1,234.5")) & ":" & Dmp(CInt("1,234.5"))
    M53 = R
    Exit Function
EH:
    M53 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M54() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = ("abcabc" Like "ABC") & "|" & ("abcabc" Like "?b?") & "|" & ("ABC" Like "abcabc")
    M54 = R
    Exit Function
EH:
    M54 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M55() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3, 1 To 2) As Long, i As Long, j As Long
    For i = 1 To 3: For j = 1 To 2: a(i, j) = i * j: Next j: Next i
    R = UBound(a, 1) & UBound(a, 2) & ":" & a(3, 2) & ":" & a(4, 3)
    M55 = R
    Exit Function
EH:
    M55 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M56() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b() As Byte, s As String, i As Long, t As String
    s = ""
    b = s
    For i = LBound(b) To UBound(b)
        t = t & b(i) & ","
    Next i
    b = StrConv(s, vbFromUnicode)
    t = t & "|" & UBound(b) & "|"
    s = b
    R = t & Hx(s) & "|" & Hx(StrConv(b, vbUnicode)) & "|" & Hx(CStr(StrConv(s, vbFromUnicode)))
    M56 = R
    Exit Function
EH:
    M56 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M57() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Chr(233)) & "|" & Dmp(ChrW(233)) & "|" & Dmp(Asc(ChrW(223))) & "|" & Dmp(AscW(ChrW(223))) & "|" & Dmp(UCase(ChrW(223))) & "|" & Dmp(LCase(ChrW(233)))
    M57 = R
    Exit Function
EH:
    M57 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M58() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Switch("x", 1, Null, 2)) & Dmp(Choose(0, "a", "b", "c")) & Dmp(IIf("x", "t", "f")) & Dmp(Partition(5, 0, 99, 10)) & Dmp(Partition(5, 1, 20, 5))
    M58 = R
    Exit Function
EH:
    M58 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M59() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Currency, b As Single
    a = fV(0.5): b = fV(256)
    R = TypeName(a > b) & ":" & Dmp(a > b)
    M59 = R
    Exit Function
EH:
    M59 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M60() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim e As R8E, v As Variant
    e = fV(ee)
    R = Dmp(e) & "|" & Dmp(e + 1) & "|" & Dmp(CStr(e)) & "|" & TypeName(e) & "|" & Dmp(e * 2) & "|" & Dmp(e And 3) & "|" & Format(e, "00") & "|" & VarType(e) & Dmp(e / 2) & Dmp(-e) & Dmp(Not e)
    M60 = R
    Exit Function
EH:
    M60 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M61() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Currency, d As Double, v As Variant
    c = fV(-0.00005@): d = fV(1E-5): v = c * d
    R = Dmp(c) & "|" & Dmp(v) & "|" & Dmp(c / 3) & "|" & Dmp(c * 3 / 3) & "|" & Dmp(c + d) & "|" & Dmp(c \ 7) & "|" & Dmp(c ^ 2)
    M61 = R
    Exit Function
EH:
    M61 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M62() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String
    s = "&HFFFF": t = "bc"
    R = InStr(s, t) & "|" & InStr(1, s, t, vbTextCompare) & "|" & InStrRev(s, t) & "|" & InStrRev(s, t, 1, vbTextCompare) & "|" & StrComp(s, t, vbTextCompare) & "|" & StrComp(s, t)
    M62 = R
    Exit Function
EH:
    M62 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M63() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b As R8Box, s As R8Sink
    R8Log = ""
    Set b = New R8Box: b.Tag = "b1"
    Set s = New R8Sink: s.Id = "s"
    Set s.Src = b
    b.Fire
    Set s.Src = New R8Box
    s.Src.Tag = "b2"
    b.Fire: s.Src.Fire
    Set b = Nothing
    R = R8Log
    Set s = Nothing
    R = R & "|" & R8Log
    R8Log = ""
    M63 = R
    Exit Function
EH:
    M63 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M64() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R8Sink
    Set s = New R8Sink
    R8Log = ""
    s.Id = "q"
    s.Src.Fire
    R = R8Log
    M64 = R
    Exit Function
EH:
    M64 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M65() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Err.Raise 70000
    s = s & Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Error(70000) & "|" & Error$(5)
    Err.Clear
    Err.Raise 70000, "mysrc", "mydesc", "hf", 77
    s = s & "|" & Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpFile & "|" & Err.HelpContext & "|" & CStr(Err) & "|" & TypeName(Err)
    Err.Clear
    Err.Description = "set"
    Err.Number = 5
    s = s & "|" & Err.Number & "|" & Err.Description
    R = s
    M65 = R
    Exit Function
EH:
    M65 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M66() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(0 To 2) As Long, s As String, i As Long
    For i = LBound(a) To UBound(a)
        a(i) = i
    Next i
    Erase a
    R = LBound(a) & ":" & UBound(a) & ":" & a(UBound(a)) & ":" & Dmp(Array("a")) & ":" & UBound(Array()) & ":" & LBound(Array(1, 2)) & ":" & Dmp(Split("a b c")) & ":" & Dmp(a(0.5))
    M66 = R
    Exit Function
EH:
    M66 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M67() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV("")
    R = TypeName(v) & VarType(v) & IsNumeric(v) & IsDate(v) & IsEmpty(v) & IsNull(v) & IsError(v) & IsObject(v) & IsArray(v) & "|" & Dmp(CVErr(7)) & "|" & Dmp(Hex(v)) & Dmp(Oct(v)) & Dmp(Str(v)) & Dmp(Val(v)) & Dmp(Len(v))
    M67 = R
    Exit Function
EH:
    M67 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M68() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, b As Variant, c As Variant
    a = CDec(fV(25)): b = CDec(fV(1)): c = CDec(fV(10))
    R = Dmp(a + b) & "|" & Dmp((a + b) + c) & "|" & Dmp(a + 1E10) & "|" & Dmp(a + CCur(c))
    M68 = R
    Exit Function
EH:
    M68 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M69() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long
    s = "" & True
    n = True & Empty
    R = Dmp(s) & Dmp(n) & Dmp(True & Empty) & Dmp(True + Empty) & Dmp(True & Null) & Dmp(True + Null)
    M69 = R
    Exit Function
EH:
    M69 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M70() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV("z")
    Select Case v
        Case ""
            R = "c0"
        Case Is >= "z"
            R = "c1"
        Case Else
            R = "else"
    End Select
    M70 = R
    Exit Function
EH:
    M70 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M71() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String, x As Variant, arr(1 To 2) As Long, o As Object
    On Error Resume Next
    For i = 1 To 2
        s = s & Err.Number & ","
        x = CInt(40000)
        s = s & Err.Number & "/" & Left$(Err.Description, 30) & ";"
        x = Space(-2)
        s = s & Err.Number & ";"
        Err.Clear
    Next i
    R = s
    M71 = R
    Exit Function
EH:
    M71 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M72() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double, n As Long, s As String
    For x = -1 To 5 Step 3
        n = n + 1
        s = s & x & ","
        If n > 12 Then Exit For
    Next x
    R = n & "|" & s & "|" & x
    M72 = R
    Exit Function
EH:
    M72 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M73() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(-2.5)
    R = TypeName(CDbl(v)) & ":" & Dmp(CDbl(v)) & "|" & VarType(v) & IsNumeric(v) & IsDate(v) & IsEmpty(v) & IsNull(v)
    M73 = R
    Exit Function
EH:
    M73 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M74() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Currency, d As Double, v As Variant
    c = fV(1.99995): d = fV(1E-5): v = c * d
    R = Dmp(c) & "|" & Dmp(v) & "|" & Dmp(c / 3) & "|" & Dmp(c * 3 / 3) & "|" & Dmp(c + d) & "|" & Dmp(c \ 7) & "|" & Dmp(c ^ 2)
    M74 = R
    Exit Function
EH:
    M74 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M75() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(1E308)
    R = FormatNumber(v, 5, 0, -1, -1) & "|" & FormatCurrency(v, 5, 0, -1, -1) & "|" & FormatPercent(v, 5, 0, -1, -1)
    M75 = R
    Exit Function
EH:
    M75 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M76() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String, x As Variant, arr(1 To 2) As Long, o As Object
    On Error Resume Next
    For i = 1 To 3
        s = s & Err.Number & ","
        Error 461
        s = s & Err.Number & "/" & Left$(Err.Description, 30) & ";"
        x = 5 Mod 0
        s = s & Err.Number & ";"
        Err.Clear
    Next i
    R = s
    M76 = R
    Exit Function
EH:
    M76 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M77() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Replace("1/2/2000", "", "", 0, 2, 5)) & "|" & Dmp(Replace("1/2/2000", "", ""))
    M77 = R
    Exit Function
EH:
    M77 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M78() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Currency, d As Double, v As Variant
    c = fV(922337203685477.5807@): d = fV(1E-5): v = c * d
    R = Dmp(c) & "|" & Dmp(v) & "|" & Dmp(c / 3) & "|" & Dmp(c * 3 / 3) & "|" & Dmp(c + d) & "|" & Dmp(c \ 7) & "|" & Dmp(c ^ 2)
    M78 = R
    Exit Function
EH:
    M78 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M79() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(1E-5)
    R = FormatNumber(v, 1, -2, 0, -2) & "|" & FormatCurrency(v, 1, -2, 0, -2) & "|" & FormatPercent(v, 1, -2, 0, -2)
    M79 = R
    Exit Function
EH:
    M79 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M80() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Switch(1, 1, True, 2)) & Dmp(Choose(Null, "a", "b", "c")) & Dmp(IIf(1, "t", "f")) & Dmp(Partition(50, 0, 99, 10)) & Dmp(Partition(50, 1, 20, 5))
    M80 = R
    Exit Function
EH:
    M80 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M81() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "#.#e+#") & "|" & Format(100, "0;0;0;""nul""") & "|" & Format(100, "#.#e+#;0;0;0;""nul""")
    M81 = R
    Exit Function
EH:
    M81 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M82() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = TypeName(CDec("100") - CDec("0.0000000000000000000000000001")) & ":" & Dmp(CDec("100") - CDec("0.0000000000000000000000000001"))
    M82 = R
    Exit Function
EH:
    M82 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M83() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Split(" a b ", "A", 3, 5)
    R = Dmp(a) & "|" & Join(a, ",")
    M83 = R
    Exit Function
EH:
    M83 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M84() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Variant, i As Long, j As Long, s As String
    ReDim a(1 To 2, 0 To 2)
    For i = 1 To 2
        For j = 0 To 2
            a(i, j) = i * 10 + j
        Next j
    Next i
    ReDim Preserve a(1 To 3, 0 To 3)
    s = UBound(a, 1) & "," & UBound(a, 2) & "," & LBound(a, 2) & ":"
    For i = 1 To UBound(a, 1)
        For j = 0 To UBound(a, 2)
            s = s & Dmp(a(i, j)) & ";"
        Next j
    Next i
    R = s & UBound(a, 0)
    M84 = R
    Exit Function
EH:
    M84 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M85() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim k As Long, s As String
    k = 4
    On k GoTo L1, L2, L3
    s = "fall"
    GoTo Fin
L1: s = "one": GoTo Fin
L2: s = "two": GoTo Fin
L3: s = "three"
Fin:
    On k GoSub S1, S2
    R = s & "|" & GsF(1)
    Exit Function
S1: s = s & "<s1>": Return
S2: s = s & "<s2>": Return
    M85 = R
    Exit Function
EH:
    M85 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M86() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, x As Variant
    On Error Resume Next
    x = IIf(False, Lg("a", True), Lg("b", False))
    s = s & R8Log & "|"
    R8Log = ""
    If Lg("x", True) And Lg("y", True) Then s = s & "T" Else s = s & "F"
    If Lg("p", True) Or Lg("q", True) Then s = s & "T" Else s = s & "F"
    R = s & R8Log
    R8Log = ""
    M86 = R
    Exit Function
EH:
    M86 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M87() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, n As Long, s As String
    i = 1
    Do While i < 5
        i = i + 3
        n = n + 1
        If n > 10 Then Exit Do
    Loop
    s = i & "," & n
    Do
        i = i - 1: n = n - 1
    Loop Until i < 1 Or n < -5
    While n < 3
        n = n + 2
    Wend
    R = s & "|" & i & "," & n
    M87 = R
    Exit Function
EH:
    M87 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M88() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, n As Long, s As String
    i = 5
    Do While i < 8
        i = i + 1
        n = n + 1
        If n > 10 Then Exit Do
    Loop
    s = i & "," & n
    Do
        i = i - 1: n = n - 1
    Loop Until i < 5 Or n < -5
    While n < 3
        n = n + 2
    Wend
    R = s & "|" & i & "," & n
    M88 = R
    Exit Function
EH:
    M88 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M89() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim e As R8E, n As Long, s As String
    For e = eb To ee
        n = n + 1
        s = s & e & ","
        If n > 20 Then Exit For
    Next e
    Select Case e
        Case ea: s = s & "A"
        Case eb To ed: s = s & "BD"
        Case Is > ec: s = s & "gt"
        Case Else: s = s & "else"
    End Select
    R = s & e
    M89 = R
    Exit Function
EH:
    M89 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M90() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single, n As Long, s As String
    For x = #1/1/2000# To 1 Step 1
        n = n + 1
        s = s & x & ","
        If n > 12 Then Exit For
    Next x
    R = n & "|" & s & "|" & x
    M90 = R
    Exit Function
EH:
    M90 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M91() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long, i As Long, s As String
    ReDim a(-1 To 5)
    For i = LBound(a) To UBound(a)
        a(i) = i * 3
    Next i
    ReDim Preserve a(1 To 4)
    For i = LBound(a) To UBound(a)
        s = s & a(i) & ","
    Next i
    R = s & LBound(a) & ":" & UBound(a) & ":" & Dmp(a)
    M91 = R
    Exit Function
EH:
    M91 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M92() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Val("+5")) & Dmp(Str(0)) & Dmp(CStr(0)) & Dmp(Trim$(Str$(0))) & Dmp(Format$(0))
    M92 = R
    Exit Function
EH:
    M92 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M93() As Variant
    On Error GoTo EH
    Dim R As Variant
    #If Not R8C Then
    R = "A"
    #ElseIf R8A + 1 = 4 Then
    R = "B"
    #Else
    R = "C"
    #End If
    #If R8D = 7 Then
    R = R & "d7"
    #End If
    #If R8N Then
    R = R & "n"
    #ElseIf Not R8C Then
    R = R & "nc"
    #Else
    R = R & "else"
    #End If
    #If VBA7 And Win64 Then
    R = R & "x64"
    #ElseIf VBA7 Then
    R = R & "v7"
    #Else
    R = R & "old"
    #End If
    #If Mac Then
    R = R & "mac"
    #End If
    #If Win32 Then
    R = R & "w32"
    #End If
    #If VBA6 Then
    R = R & "v6"
    #End If
    M93 = R
    Exit Function
EH:
    M93 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M94() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object, v As Variant, s As String
    Set o = New R8Box: s = o.Twice("x")
    R = s
    M94 = R
    Exit Function
EH:
    M94 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M95() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(StrConv("#1/2/2000#", 64)) & "|" & Dmp(StrConv("#1/2/2000#", 64, 1033))
    M95 = R
    Exit Function
EH:
    M95 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M96() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = GsF(0) & GsF(1)
    M96 = R
    Exit Function
EH:
    M96 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M97() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection, s As String, v As Variant
    On Error Resume Next
    With c
        .Add 1, "a"
        .Add 2, "a"
        s = s & Err.Number & ","
        .Add 3
        s = s & .Item(1) & "," & Err.Number & ","
        s = s & .Item("zz") & "," & Err.Number & ","
        .Remove 1
        s = s & .Count & "," & Err.Number
    End With
    R = s
    M97 = R
    Exit Function
EH:
    M97 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M98() As Variant
    On Error GoTo EH
    Dim R As Variant
    #If R8A Mod 2 = 1 And R8C Then
    R = "A"
    #ElseIf R8A + 1 = 4 Then
    R = "B"
    #Else
    R = "C"
    #End If
    #If R8D = 7 Then
    R = R & "d7"
    #End If
    #If R8N Then
    R = R & "n"
    #ElseIf Not R8C Then
    R = R & "nc"
    #Else
    R = R & "else"
    #End If
    #If VBA7 And Win64 Then
    R = R & "x64"
    #ElseIf VBA7 Then
    R = R & "v7"
    #Else
    R = R & "old"
    #End If
    #If Mac Then
    R = R & "mac"
    #End If
    #If Win32 Then
    R = R & "w32"
    #End If
    #If VBA6 Then
    R = R & "v6"
    #End If
    M98 = R
    Exit Function
EH:
    M98 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M99() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b As R8Box, s1 As R8Sink, s2 As R8Sink
    Set b = New R8Box
    Set s1 = New R8Sink: s1.Id = "1": Set s1.Src = b
    Set s2 = New R8Sink: s2.Id = "2": Set s2.Src = b
    R8Log = ""
    s1.Veto = False
    s2.Boom = 0
    s2.Detach = True
    On Error Resume Next
    b.N = 5
    R = R8Log & "|" & b.N & "|" & Err.Number
    Err.Clear
    b.Fire
    R = R & "|" & R8Log & "|" & b.FireMsg("m") & "|" & Err.Number
    Set s1 = Nothing
    Set b = Nothing
    R = R & "|" & R8Log
    R8Log = ""
    M99 = R
    Exit Function
EH:
    M99 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M100() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StatCnt() & StatCnt() & StatCnt()
    M100 = R
    Exit Function
EH:
    M100 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M101() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, j As Long, n As Long, s As String, lim As Long
    lim = 3
    For i = 1 To lim
        lim = 10
        i = i + 0
        s = s & i & ","
        n = n + 1
        If n > 15 Then Exit For
    Next i
    R = s & n & ":" & i & ":" & lim
    M101 = R
    Exit Function
EH:
    M101 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M102() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(StrConv("12", 32)) & "|" & Dmp(StrConv("12", 32, 1033))
    M102 = R
    Exit Function
EH:
    M102 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M103() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Switch(Null, 1, False, 2)) & Dmp(Choose(2.5, "a", "b", "c")) & Dmp(IIf(Null, "t", "f")) & Dmp(Partition(Null, 0, 99, 10)) & Dmp(Partition(Null, 1, 20, 5))
    M103 = R
    Exit Function
EH:
    M103 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M104() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long, i As Long, s As String
    ReDim a(-1 To 3)
    For i = LBound(a) To UBound(a)
        a(i) = i * 3
    Next i
    ReDim Preserve a(-1 To 0)
    For i = LBound(a) To UBound(a)
        s = s & a(i) & ","
    Next i
    R = s & LBound(a) & ":" & UBound(a) & ":" & Dmp(a)
    M104 = R
    Exit Function
EH:
    M104 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M105() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abcabc"
    R = Dmp(Left(s, 2)) & Dmp(Right(s, 2)) & Dmp(Mid(s, 2)) & Dmp(Mid(s, 2, 2)) & Dmp(Trim(s)) & Dmp(LTrim(s)) & Dmp(RTrim(s)) & Dmp(UCase(s)) & Dmp(LCase(s)) & Dmp(StrReverse(s)) & Len(s) & "," & LenB(s)
    M105 = R
    Exit Function
EH:
    M105 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M106() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, b As Date
    a = fV(7): b = fV(3)
    R = TypeName(a Or b) & ":" & Dmp(a Or b)
    M106 = R
    Exit Function
EH:
    M106 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M107() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Variant
    d = fV(#12/31/1899#)
    R = Dmp(Year(d)) & Dmp(Month(d)) & Dmp(Day(d)) & Dmp(Hour(d)) & Dmp(Minute(d)) & Dmp(Second(d)) & Dmp(Weekday(d, 2)) & Dmp(DateValue(d)) & Dmp(TimeValue(d)) & Dmp(CDbl(d)) & Dmp(Int(d))
    M107 = R
    Exit Function
EH:
    M107 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M108() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Single, b As Variant
    a = fV(#12/31/9999#): b = fV(32768)
    R = TypeName(a * b) & ":" & Dmp(a * b)
    M108 = R
    Exit Function
EH:
    M108 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M109() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Integer, v As Variant, n As Long
    On Error Resume Next
    n = UBound(a)
    R = n & ":" & Err.Number & ":" & LBound(a) & ":" & Err.Number & ":" & IsArray(a) & ":" & IsArray(v)
    v = a
    R = R & ":" & IsArray(v) & ":" & TypeName(v) & ":" & TypeName(a) & ":" & VarType(a)
    ReDim a(0)
    R = R & ":" & UBound(a) & ":" & TypeName(a) & ":" & VarType(a) & ":" & Len(a(0))
    M109 = R
    Exit Function
EH:
    M109 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M110() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(32768)
    Select Case v
        Case 2.5 To 2.5, Is > 50
            R = "c0"
        Case Is <> 1
            R = "c1"
        Case Is <= -1
            R = "c2"
        Case Else
            R = "else"
    End Select
    R = R & TypeName(v)
    M110 = R
    Exit Function
EH:
    M110 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M111() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, b As Long
    a = fV(-129): b = fV(256)
    R = TypeName(a Imp b) & ":" & Dmp(a Imp b)
    M111 = R
    Exit Function
EH:
    M111 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M112() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long, i As Long, s As String
    ReDim a(2 To 2)
    For i = LBound(a) To UBound(a)
        a(i) = i * 3
    Next i
    ReDim Preserve a(1 To 0)
    For i = LBound(a) To UBound(a)
        s = s & a(i) & ","
    Next i
    R = s & LBound(a) & ":" & UBound(a) & ":" & Dmp(a)
    M112 = R
    Exit Function
EH:
    M112 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M113() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Variant
    d = fV(#1/1/100#)
    R = Format(d, "Long Date") & "|" & Format(d, "AM/PM") & "|" & FormatDateTime(d, 3)
    M113 = R
    Exit Function
EH:
    M113 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M114() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Object, s As String, v As Variant
    Set d = CreateObject("Scripting.Dictionary")
    d.CompareMode = 2
    d.Add "a", 1
    d.Add "B", "two"
    d.Add 3, Null
    d.Add 3.5, Array(1, 2)
    d("new") = 5
    s = s & d.Count & ":" & Join(d.Keys, ",") & ":" & d.CompareMode
    R = s
    M114 = R
    Exit Function
EH:
    M114 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M115() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a"
    Return
    M115 = R
    Exit Function
EH:
    M115 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M116() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim f As Object
    Set f = CreateObject("Scripting.FileSystemObject")
    R = f.GetBaseName("c:\") & "|" & f.GetExtensionName("c:\") & "|" & f.GetFileName("c:\") & "|" & f.GetParentFolderName("c:\") & "|" & f.BuildPath("c:\", "..\z") & "|" & f.FileExists("") & f.FolderExists("") & f.GetDriveName("c:\")
    M116 = R
    Exit Function
EH:
    M116 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M117() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(StrConv("", 1+2)) & "|" & Dmp(StrConv("", 1+2, 1033))
    M117 = R
    Exit Function
EH:
    M117 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M118() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, n As Long, s As String
    i = 1
    Do While i < 5
        i = i + 2
        n = n + 1
        If n > 10 Then Exit Do
    Loop
    s = i & "," & n
    Do
        i = i - 1: n = n - 1
    Loop Until i < 1 Or n < -5
    While n < 3
        n = n + 2
    Wend
    R = s & "|" & i & "," & n
    M118 = R
    Exit Function
EH:
    M118 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M119() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3, 1 To 2) As Long, i As Long, j As Long
    For i = 1 To 3: For j = 1 To 2: a(i, j) = i * j: Next j: Next i
    R = UBound(a, 1) & UBound(a, 2) & ":" & a(3, 2) & ":" & a(4, 2)
    M119 = R
    Exit Function
EH:
    M119 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M120() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a"
    Return
    M120 = R
    Exit Function
EH:
    M120 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M121() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, j As Long, n As Long, s As String, lim As Long
    lim = -1
    For i = 1 To lim
        lim = 10
        i = i + 2
        s = s & i & ","
        n = n + 1
        If n > 15 Then Exit For
    Next i
    R = s & n & ":" & i & ":" & lim
    M121 = R
    Exit Function
EH:
    M121 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M122() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String
    s = "": t = "A"
    R = InStr(s, t) & "|" & InStr(100, s, t, vbTextCompare) & "|" & InStrRev(s, t) & "|" & InStrRev(s, t, 100, vbTextCompare) & "|" & StrComp(s, t, vbTextCompare) & "|" & StrComp(s, t)
    M122 = R
    Exit Function
EH:
    M122 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M123() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, c As Collection, s As String, n As Long
    Set c = New Collection
    c.Add 1: c.Add 2: c.Add 3
    For Each v In c
        s = s & v & ","
        n = n + 1
        If v = 3 Then c.Remove c.Count
        If n > 8 Then Exit For
    Next v
    R = s & c.Count
    M123 = R
    Exit Function
EH:
    M123 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M124() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R8Sink
    Set s = New R8Sink
    R8Log = ""
    s.Id = "q"
    s.Src.Fire
    R = R8Log
    M124 = R
    Exit Function
EH:
    M124 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M125() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, n As Long, s As String
    i = 5
    Do While i < 3
        i = i + 2
        n = n + 1
        If n > 10 Then Exit Do
    Loop
    s = i & "," & n
    Do
        i = i - 1: n = n - 1
    Loop Until i < 5 Or n < -5
    While n < 3
        n = n + 2
    Wend
    R = s & "|" & i & "," & n
    M125 = R
    Exit Function
EH:
    M125 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M126() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long, s As String
    x = 3
    If x > 3 Then s = "gt": If x > 6 Then s = s & "6"
    If x = 1 Then s = s & "one" Else If x = 2 Then s = s & "two" Else s = s & "oth"
    If x > 0 Then
        s = s & "p"
    ElseIf x = 0 Then
        s = s & "z"
    Else
        s = s & "n"
    End If
    R = s
    M126 = R
    Exit Function
EH:
    M126 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M127() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Replace("Hello World", " ", "-", 1, -1, vbTextCompare)) & "|" & Dmp(Replace("Hello World", " ", "-"))
    M127 = R
    Exit Function
EH:
    M127 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M128() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Variant, i As Long, j As Long, s As String
    ReDim a(1 To 3, 0 To 1)
    For i = 1 To 3
        For j = 0 To 1
            a(i, j) = i * 10 + j
        Next j
    Next i
    ReDim Preserve a(1 To 2, 0 To 3)
    s = UBound(a, 1) & "," & UBound(a, 2) & "," & LBound(a, 2) & ":"
    For i = 1 To UBound(a, 1)
        For j = 0 To UBound(a, 2)
            s = s & Dmp(a(i, j)) & ";"
        Next j
    Next i
    R = s & UBound(a, 2)
    M128 = R
    Exit Function
EH:
    M128 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M129() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, b As Variant, c As Variant
    a = CDec(fV(12)): b = CDec(fV(CInt(-5))): c = CDec(fV(922337203685477.5807@))
    R = Dmp(a Xor b) & "|" & Dmp((a / b) / c) & "|" & Dmp(a / 1E10) & "|" & Dmp(a / CCur(c))
    M129 = R
    Exit Function
EH:
    M129 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M130() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b() As Byte, s As String, i As Long, t As String
    s = "x"
    b = s
    For i = LBound(b) To UBound(b)
        t = t & b(i) & ","
    Next i
    b = StrConv(s, vbFromUnicode)
    t = t & "|" & UBound(b) & "|"
    s = b
    R = t & Hx(s) & "|" & Hx(StrConv(b, vbUnicode)) & "|" & Hx(CStr(StrConv(s, vbFromUnicode)))
    M130 = R
    Exit Function
EH:
    M130 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M131() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(-0.5)
    R = TypeName(Int(v)) & ":" & Dmp(Int(v)) & "|" & VarType(v) & IsNumeric(v) & IsDate(v) & IsEmpty(v) & IsNull(v)
    M131 = R
    Exit Function
EH:
    M131 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M132() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b As R8Box, s1 As R8Sink, s2 As R8Sink
    Set b = New R8Box
    Set s1 = New R8Sink: s1.Id = "1": Set s1.Src = b
    Set s2 = New R8Sink: s2.Id = "2": Set s2.Src = b
    R8Log = ""
    s1.Veto = True
    s2.Boom = 0
    s2.Detach = False
    On Error Resume Next
    b.N = 5
    R = R8Log & "|" & b.N & "|" & Err.Number
    Err.Clear
    b.Fire
    R = R & "|" & R8Log & "|" & b.FireMsg("m") & "|" & Err.Number
    Set s1 = Nothing
    Set b = Nothing
    R = R & "|" & R8Log
    R8Log = ""
    M132 = R
    Exit Function
EH:
    M132 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M133() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, """x""0") & "|" & Format(-0.5, "Currency") & "|" & Format(-0.5, """x""0;Currency")
    M133 = R
    Exit Function
EH:
    M133 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M134() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(StrConv("", 1)) & "|" & Dmp(StrConv("", 1, 1033))
    M134 = R
    Exit Function
EH:
    M134 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M135() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV("b")
    Select Case v
        Case Is < "z"
            R = "c0"
        Case Is < "m"
            R = "c1"
        Case "m" To "z"
            R = "c2"
        Case Else
            R = "else"
    End Select
    M135 = R
    Exit Function
EH:
    M135 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M136() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim b() As Byte, s As String, i As Long, t As String
    s = "AbC"
    b = s
    For i = LBound(b) To UBound(b)
        t = t & b(i) & ","
    Next i
    b = StrConv(s, vbFromUnicode)
    t = t & "|" & UBound(b) & "|"
    s = b
    R = t & Hx(s) & "|" & Hx(StrConv(b, vbUnicode)) & "|" & Hx(CStr(StrConv(s, vbFromUnicode)))
    M136 = R
    Exit Function
EH:
    M136 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M137() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dmp(Chr(181)) & "|" & Dmp(ChrW(181)) & "|" & Dmp(Asc(Chr(200))) & "|" & Dmp(AscW(Chr(200))) & "|" & Dmp(UCase(Chr(200))) & "|" & Dmp(LCase(ChrW(181)))
    M137 = R
    Exit Function
EH:
    M137 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M138() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Opt1(Null)
    M138 = R
    Exit Function
EH:
    M138 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M139() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, j As Long, n As Long, s As String, lim As Long
    lim = 5
    For i = 1 To lim
        lim = 10
        i = i + 2
        s = s & i & ","
        n = n + 1
        If n > 15 Then Exit For
    Next i
    R = s & n & ":" & i & ":" & lim
    M139 = R
    Exit Function
EH:
    M139 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M140() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(CInt(-5))
    R = TypeName(CBool(v)) & ":" & Dmp(CBool(v)) & "|" & VarType(v) & IsNumeric(v) & IsDate(v) & IsEmpty(v) & IsNull(v)
    M140 = R
    Exit Function
EH:
    M140 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M141() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = fV(2147483647)
    R = FormatNumber(v, 5, -2, 0, 0) & "|" & FormatCurrency(v, 5, -2, 0, 0) & "|" & FormatPercent(v, 5, -2, 0, 0)
    M141 = R
    Exit Function
EH:
    M141 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M142() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long, i As Long, s As String
    ReDim a(0 To 5)
    For i = LBound(a) To UBound(a)
        a(i) = i * 3
    Next i
    ReDim Preserve a(-1 To 0)
    For i = LBound(a) To UBound(a)
        s = s & a(i) & ","
    Next i
    R = s & LBound(a) & ":" & UBound(a) & ":" & Dmp(a)
    M142 = R
    Exit Function
EH:
    M142 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M143() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Opt1(, 5)
    M143 = R
    Exit Function
EH:
    M143 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M144() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error GoTo EH2
    Thrower vbObjectError + 1
    s = s & "after"
    R = s
    Exit Function
EH2:
    R = s & "H" & Err.Number & ":" & Err.Source & ":" & Err.Description & ":" & Err.HelpContext & ":" & Err.LastDllError
    On Error GoTo 0
    M144 = R
    Exit Function
EH:
    M144 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M145() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Integer, v As Variant, n As Long
    On Error Resume Next
    n = UBound(a)
    R = n & ":" & Err.Number & ":" & LBound(a) & ":" & Err.Number & ":" & IsArray(a) & ":" & IsArray(v)
    v = a
    R = R & ":" & IsArray(v) & ":" & TypeName(v) & ":" & TypeName(a) & ":" & VarType(a)
    ReDim a(0)
    R = R & ":" & UBound(a) & ":" & TypeName(a) & ":" & VarType(a) & ":" & Len(a(0))
    M145 = R
    Exit Function
EH:
    M145 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M146() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection, s As String, v As Variant
    c.Add "a", "ka"
    c.Add "b", "kb"
    c.Add "c", "kc", 1
    c.Add "d", "kd", , 2
    s = c(1.5) & c("x")
    For Each v In c
        s = s & v
    Next v
    R = s & ":" & c.Count
    M146 = R
    Exit Function
EH:
    M146 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M147() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "0;0;0;""nul""") & "|" & Format(0, "0.###") & "|" & Format(0, "0;0;0;""nul"";0.###")
    M147 = R
    Exit Function
EH:
    M147 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M148() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long
    s = "" & 1
    n = 1 & "2"
    R = Dmp(s) & Dmp(n) & Dmp(1 & "2") & Dmp(1 + "2") & Dmp(1 & Null) & Dmp(1 + Null)
    M148 = R
    Exit Function
EH:
    M148 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M149() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Split("aBc", "abc", 3, 2)
    R = Dmp(a) & "|" & Join(a, ",")
    M149 = R
    Exit Function
EH:
    M149 = "#ERR " & Err.Number & " " & Err.Description
End Function
