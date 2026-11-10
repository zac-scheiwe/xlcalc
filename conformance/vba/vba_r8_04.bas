Attribute VB_Name = "VbaR8_04"
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
Public Function VbEight4(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k \ 50
    Case 0: VbEight4 = MC0(k)
    End Select
    Exit Function
EH:
    VbEight4 = "#ERR " & Err.Number & " " & Err.Description
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
    End Select
    Exit Function
EH:
    MC0 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M0() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 3)
    a(3).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M0 = R
    Exit Function
EH:
    M0 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M1() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 2)
    a(2).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M1 = R
    Exit Function
EH:
    M1 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M2() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 3)
    a(3).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M2 = R
    Exit Function
EH:
    M2 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M3() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 1)
    a(1).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M3 = R
    Exit Function
EH:
    M3 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M4() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 3)
    a(3).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M4 = R
    Exit Function
EH:
    M4 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M5() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 2)
    a(2).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M5 = R
    Exit Function
EH:
    M5 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M6() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 3)
    a(3).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M6 = R
    Exit Function
EH:
    M6 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M7() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 1)
    a(1).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M7 = R
    Exit Function
EH:
    M7 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M8() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 2)
    a(2).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M8 = R
    Exit Function
EH:
    M8 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M9() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 1)
    a(1).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M9 = R
    Exit Function
EH:
    M9 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M10() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 1)
    a(1).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M10 = R
    Exit Function
EH:
    M10 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M11() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As R8P, i As Long, s As String
    ReDim a(1 To 2)
    a(1).X = 5
    ReDim Preserve a(1 To 1)
    a(1).S = "last"
    For i = 1 To UBound(a)
        s = s & a(i).X & ":" & a(i).S & ":" & a(i).B(1) & ","
    Next i
    R = s
    M11 = R
    Exit Function
EH:
    M11 = "#ERR " & Err.Number & " " & Err.Description
End Function
