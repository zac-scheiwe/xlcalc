Attribute VB_Name = "VbaR9_01"
Option Compare Text
Option Explicit
Private Type SP
    A As Long
    B As String * 3
    C(1 To 2) As Integer
End Type
Private Type SQ
    P As SP
    Q As Double
    V As Variant
End Type
Private Type SOdd
    A As Byte
    B As Long
    C As Integer
    D As Double
    E As Boolean
    F As String * 2
End Type
Private Declare PtrSafe Function lstrlenA Lib "kernel32" (ByVal s As String) As Long
Private Declare PtrSafe Function lstrcmpA Lib "kernel32" (ByVal a As String, ByVal b As String) As Long
Private Declare PtrSafe Function lstrcmpiA Lib "kernel32" (ByVal a As String, ByVal b As String) As Long
Private Declare PtrSafe Function MulDiv Lib "kernel32" (ByVal a As Long, ByVal b As Long, ByVal c As Long) As Long
Private Declare PtrSafe Function GetACP Lib "kernel32" () As Long
Private Declare PtrSafe Function IsCharAlphaA Lib "user32" (ByVal c As Byte) As Long
Private Declare PtrSafe Function IsCharUpperA Lib "user32" (ByVal c As Byte) As Long
Private Declare PtrSafe Function IsCharLowerA Lib "user32" (ByVal c As Byte) As Long
Private Declare PtrSafe Function AbsC Lib "msvcrt" Alias "abs" (ByVal n As Long) As Long
Private Declare PtrSafe Function NoFn Lib "kernel32" Alias "NoSuchFunction9" () As Long
Private Declare PtrSafe Function NoLib Lib "nosuchlib9.dll" () As Long
Private Static_n As Long
Private m_keep As R9Impl
Private m_keepSrc As R9Src
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
    ElseIf VarType(v) = vbError Then
        Dmp = "Err:" & CLng(v)
    Else
        Dmp = TypeName(v) & ":" & Hx(CStr(v))
    End If
End Function
Private Function DmpX(v As Variant) As String
    Dim i As Long, j As Long, s As String, n As Long
    If Not IsArray(v) Then DmpX = Dmp(v): Exit Function
    On Error Resume Next
    n = UBound(v, 2)
    If Err.Number <> 0 Then
        Err.Clear
        DmpX = Dmp(v)
        Exit Function
    End If
    On Error GoTo 0
    s = "M" & LBound(v, 1) & ":" & UBound(v, 1) & "," & LBound(v, 2) & ":" & UBound(v, 2) & "["
    For i = LBound(v, 1) To UBound(v, 1)
        For j = LBound(v, 2) To UBound(v, 2)
            s = s & Dmp(v(i, j)) & ","
        Next j
        s = s & ";"
    Next i
    DmpX = s & "]"
End Function
Private Sub ByRefL(ByRef n As Long)
    n = n + 1
End Sub
Private Sub ByRefB(ByRef n As Byte)
    n = n + 1
End Sub
Private Sub ByRefS(ByRef s As String)
    s = s & "z"
End Sub
Public Function RunA(Optional a As Variant, Optional b As Variant) As String
    RunA = "A:" & IIf(IsMissing(a), "M", TypeName(a) & CStr(a)) & ":" & IIf(IsMissing(b), "M", TypeName(b) & CStr(b))
End Function
Public Function RunRef(ByRef n As Long) As Long
    n = n + 1
    RunRef = n * 10
End Function
Public Function RunVRef(ByRef v As Variant) As String
    RunVRef = TypeName(v)
    v = "changed"
End Function
Public Function RunPA(ParamArray p() As Variant) As String
    RunPA = UBound(p) - LBound(p) + 1 & ":" & p(0)
End Function
Public Function RunArr(v As Variant) As String
    RunArr = DmpX(v)
End Function
Public Sub RunSub()
    R9Log = R9Log & "[sub]"
End Sub
Public Function RunErr(ByVal n As Long) As Long
    On Error Resume Next
    Err.Raise n
    RunErr = Err.Number
End Function
Public Function RunLog(ByVal s As String) As String
    R9Log = R9Log & s
    RunLog = s
End Function
Private Function StatCnt() As Long
    Static n As Long
    n = n + 1
    StatCnt = n
End Function
Private Function StatStr(ByVal s As String) As String
    Static acc As String, calls As Long
    acc = acc & s
    calls = calls + 1
    StatStr = acc & calls
End Function
Private Function StatUdt() As String
    Static p As SP
    p.A = p.A + 1
    p.B = p.B & "x"
    p.C(2) = p.C(2) + 2
    StatUdt = p.A & p.B & p.C(2)
End Function
Private Static Function StatAll() As String
    Dim q As Long, z As String
    q = q + 1
    z = z & "y"
    StatAll = q & z
End Function
Private Sub ModBump()
    Static_n = Static_n + 1
End Sub
Private Sub ScopeA(ByVal mode As Long)
    Dim a As R9Impl, b As R9Impl, c As New R9Impl
    Set a = New R9Impl: a.Tag = "a"
    Set b = New R9Impl: b.Tag = "b"
    c.Tag = "c"
    R9Log = R9Log & "|"
    If mode = 1 Then Set a = b
    If mode = 2 Then Set b = Nothing
    If mode = 3 Then Set a = Nothing: Set b = Nothing
    If mode = 4 Then Set m_keep = a
    If mode = 5 Then Exit Sub
    R9Log = R9Log & "e"
End Sub
Private Sub ScopeB(ByVal n As Long)
    Dim arr(1 To 3) As R9Impl, i As Long, col As New Collection
    For i = 1 To n
        Set arr(i) = New R9Impl
        arr(i).Tag = CStr(i)
        col.Add arr(i)
    Next i
    R9Log = R9Log & "|"
    Set arr(1) = Nothing
    R9Log = R9Log & "|"
    Set col = Nothing
    R9Log = R9Log & "e"
End Sub
Private Sub ScopeC()
    Dim x As New R9Impl
    R9Log = R9Log & "a"
    x.Tag = "x"
    Set x = Nothing
    R9Log = R9Log & "b"
    x.Tag = "y"
    R9Log = R9Log & "c"
End Sub
Private Function ByRefLF(ByRef n As Long) As Long
    n = n + 1
    ByRefLF = n
End Function
Private Function Z0() As Long
    Z0 = 0
End Function
Public Function VbNine1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k \ 50
    Case 0: VbNine1 = MC0(k)
    Case 1: VbNine1 = MC1(k)
    Case 2: VbNine1 = MC2(k)
    End Select
    Exit Function
EH:
    VbNine1 = "#ERR " & Err.Number & " " & Err.Description
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
    End Select
    Exit Function
EH:
    MC2 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M0() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\w01_22016.dat"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    f = FreeFile
    Open fn For Random As #f Len = Len(rc)
    SetRec rc, 1, "one"
    Put #f, 1, rc
    SetRec rc, 2, "two"
    Put #f, 2, rc
    s = LOF(f) & "|" & Len(rc) & "|"
    Get #f, 1, rc2
    s = s & RecStr(rc2) & "|" & Loc(f) & "|" & EOF(f)
    Close #f
    Kill fn
    R = s
    M0 = R
    Exit Function
EH:
    M0 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M1() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, c As New R9Cls
    R9Log = ""
    With rc
        .Id = 3
        With .Pt
            .X = 13
            With c
                .Tag = "t"
                .SetPt rc.Pt
            End With
            .S = "in"
        End With
        .Nm = "nm"
        .Tags(2) = .Id + .Pt.X
    End With
    Dim p As R9Pt
    p = c.Pt
    R = RecStr(rc) & "|" & PtStr(p)
    M1 = R
    Exit Function
EH:
    M1 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M2() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("SUM({1,2,3})")
    R = DmpX(v) & "|" & Err.Number & "|" & TypeName(v)
    M2 = R
    Exit Function
EH:
    M2 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M3() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    R = Asc("Hello World") & "|" & Err.Number & "|" & AscW("Hello World") & "|" & AscB("Hello World") & "|" & Err.Number & "|" & Hx(Chr(141)) & "|" & Err.Number & "|" & Hx(ChrW(141)) & "|" & Err.Number & "|" & Hx(ChrB(141)) & "|" & Err.Number & "|" & LenB(Chr(141)) & LenB(ChrW(141)) & LenB(ChrB(141)) & Err.Number
    M3 = R
    Exit Function
EH:
    M3 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M4() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim q As New Collection, v As Variant, s As String
    q.Add "RunLog": q.Add "RunErr": q.Add "RunLog": q.Add "NoSuch9": q.Add "RunA"
    R9Log = ""
    On Error Resume Next
    For Each v In q
        Err.Clear
        s = s & v & "=" & Dmp(Application.Run(v, 1)) & "/" & Err.Number & ";"
    Next v
    R = s & R9Log
    M4 = R
    Exit Function
EH:
    M4 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M5() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\w01_42610.dat"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    f = FreeFile
    Open fn For Random As #f Len = Len(rc)
    SetRec rc, 1, "one"
    Put #f, 1, rc
    SetRec rc, 2, "two"
    Put #f, 5, rc
    s = LOF(f) & "|" & Len(rc) & "|"
    Get #f, 2, rc2
    s = s & RecStr(rc2) & "|" & Loc(f) & "|" & EOF(f)
    Close #f
    Kill fn
    R = s
    M5 = R
    Exit Function
EH:
    M5 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M6() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim n As Long, s As String, i As Long
    n = 0
    GoSub L3
    GoTo Fin
L1:
    s = s & "1" & n
    n = n + 1
    Return
L2:
    s = s & "2"
    GoSub L1
    s = s & "b"
    Return
L3:
    s = s & "3"
    If n > 2 Then Return
    s = s & "x"
    GoSub L3
    Return
Fin:
    R = s & "|" & n
    M6 = R
    Exit Function
EH:
    M6 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M7() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, f As R9Fix, p As R9Pt, s As SP, q As SQ, o As SOdd
    R = Len(rc) & "," & LenB(rc) & "," & Len(f) & "," & LenB(f) & "," & Len(p) & "," & LenB(p) & "," & Len(s) & "," & LenB(s) & "," & Len(q) & "," & LenB(q) & "," & Len(o) & "," & LenB(o) & "," & Len(rc.Pt) & "," & LenB(rc.Nm) & "," & Len(rc.Nm) & "," & LenB(q.P)
    M7 = R
    Exit Function
EH:
    M7 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M8() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 3
        GoSub Body
    Next i
    GoTo Fin
Body:
    Select Case i
        Case 1: s = s & "one"
        Case 2: GoSub Inner: s = s & "two"
        Case Else: s = s & "other"
    End Select
    Return
Inner:
    s = s & "(in" & i & ")"
    Return
Fin:
    R = s
    M8 = R
    Exit Function
EH:
    M8 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M9() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, i As Long, s As String
    For i = 1 To 3
        rc.Tags(i) = 255
    Next i
    rc2 = rc: ByRefB rc2.Tags(3)
    For i = LBound(rc2.Tags) To UBound(rc2.Tags)
        s = s & rc2.Tags(i) & ","
    Next i
    R = s & RecStr(rc)
    M9 = R
    Exit Function
EH:
    M9 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M10() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, a() As R9Rec, i As Long
    ReDim a(1 To 2)
    For i = 1 To 2
        SetRec a(i), i * 40, "n" & i
    Next i
    SetRec rc, 9, "nine"
    ReDim Preserve a(1 To 4): a(4) = rc
    R = RecArrStr(a) & "|" & RecStr(rc) & "|" & LBound(a) & UBound(a)
    M10 = R
    Exit Function
EH:
    M10 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M11() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 3
        GoSub Body
    Next i
    GoTo Fin
Body:
    Select Case i
        Case 1: s = s & "one"
        Case 2: GoSub Inner: s = s & "two"
        Case Else: s = s & "other"
    End Select
    Return
Inner:
    s = s & "(in" & i & ")"
    Return
Fin:
    R = s
    M11 = R
    Exit Function
EH:
    M11 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M12() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, f As R9Fix, p As R9Pt, s As SP, q As SQ, o As SOdd
    R = Len(rc) & "," & LenB(rc) & "," & Len(f) & "," & LenB(f) & "," & Len(p) & "," & LenB(p) & "," & Len(s) & "," & LenB(s) & "," & Len(q) & "," & LenB(q) & "," & Len(o) & "," & LenB(o) & "," & Len(rc.Pt) & "," & LenB(rc.Nm) & "," & Len(rc.Nm) & "," & LenB(q.P)
    M12 = R
    Exit Function
EH:
    M12 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M13() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.ExecuteExcel4Macro("1+2")
    R = DmpX(v) & "|" & Err.Number & "|" & Err.Description
    M13 = R
    Exit Function
EH:
    M13 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M14() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String
    GPt = MkPt(1, 2, "g")
    For i = 1 To 3: SetRec GRecs(i), i, "rc" & i: Next i
    R = PtStr(GPt) & "|" & RecArrStr(GRecs) & "|" & Static_n
    M14 = R
    Exit Function
EH:
    M14 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M15() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, c As New R9Cls
    R9Log = ""
    With rc
        .Id = 3
        With .Pt
            .X = 12
            With c
                .Tag = "t"
                .SetPt rc.Pt
            End With
            .S = "in"
        End With
        .Nm = "nm"
        .Tags(2) = .Id + .Pt.X
    End With
    Dim p As R9Pt
    p = c.Pt
    R = RecStr(rc) & "|" & PtStr(p)
    M15 = R
    Exit Function
EH:
    M15 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M16() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("ISERROR(1/0)")
    R = DmpX(v) & "|" & DmpX(Application.Evaluate("{1,2}*{3;4}")) & "|" & Err.Number
    M16 = R
    Exit Function
EH:
    M16 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M17() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\w01_19071.dat"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    f = FreeFile
    Open fn For Random As #f Len = Len(rc)
    SetRec rc, 1, "one"
    Put #f, 1, rc
    SetRec rc, 2, "two"
    Put #f, 2, rc
    s = LOF(f) & "|" & Len(rc) & "|"
    Get #f, 1, rc2
    s = s & RecStr(rc2) & "|" & Loc(f) & "|" & EOF(f)
    Close #f
    Kill fn
    R = s
    M17 = R
    Exit Function
EH:
    M17 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M18() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\v01_69152.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    p = MkPt(4, 2.5, "s")
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , p
    s = LOF(f) & "|"
    Seek #f, 1
    Get #f, , p2
    s = s & Loc(f) & "|" & PtStr(p2)
    Close #f
    Kill fn
    R = s
    M18 = R
    Exit Function
EH:
    M18 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M19() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String, n As Long
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\u01_75848.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    SetRec rc, 0, "ab" & 0
    p = MkPt(0, 1.5, "var")
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , rc: Seek #f, 1: Get #f, , rc2: Seek #f, 1: Put #f, , rc2
    n = LOF(f)
    Seek #f, 1
    Get #f, , rc2
    s = RecStr(rc2) & "|" & Loc(f) & "|" & LOF(f) & "|" & EOF(f)
    Close #f
    Kill fn
    R = s & "|" & PtStr(p2) & "|" & n
    M19 = R
    Exit Function
EH:
    M19 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M20() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim s As String
    s = ChrW(955) & "x"
    R = lstrlenA(s) & "|" & lstrlenA(ChrW(955) & "x") & "|" & MulDiv(2147483647, 1, 1000) & "|" & Sgn(lstrcmpA(ChrW(955) & "x", "caf" & ChrW(233))) & Sgn(lstrcmpiA(ChrW(955) & "x", "caf" & ChrW(233))) & "|" & GetACP() & "|" & IsCharAlphaA(233) & IsCharUpperA(233) & IsCharLowerA(233) & "|" & AbsC(2147483647) & "|" & Err.Number
    M20 = R
    Exit Function
EH:
    M20 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M21() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, i As Long
    For i = 1 To 1
        c.Grow i + 2
    Next i
    R = c.DynStr()
    M21 = R
    Exit Function
EH:
    M21 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M22() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, b() As Byte, i As Long, t As String
    s = "Hello World"
    b = s
    For i = LBound(b) To UBound(b)
        t = t & Hex(b(i)) & "."
    Next i
    R = t & "|" & Hx(b) & "|" & UBound(b) & "|" & Hx(StrConv(b, vbUnicode)) & "|" & Hx(StrConv(b, vbFromUnicode)) & "|" & Hx(CStr(b))
    M22 = R
    Exit Function
EH:
    M22 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M23() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, p As R9Pt
    c.SetEl 2, MkPt(5, 1.5, "e")
    With c
        .Tag = "w"
        .BumpEl 2
        R = .WithTest() & .ElStr(2)
        With .Pt
            R = R & "."
        End With
    End With
    R = R & c.Tag
    M23 = R
    Exit Function
EH:
    M23 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M24() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.ExecuteExcel4Macro("COUNTA(Sheet1!R1C1:R3C3)")
    R = DmpX(v) & "|" & Err.Number & "|" & Err.Description
    M24 = R
    Exit Function
EH:
    M24 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M25() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, v As Variant, w As Variant
    a = Array(CByte(1),CCur(2),CDec(3))
    On Error Resume Next
    v = Application.Median(a)
    R = DmpX(v) & "|" & Err.Number
    Err.Clear
    w = Application.WorksheetFunction.Median(a)
    R = R & "|" & DmpX(w) & "|" & Err.Number & "|" & Left(Err.Description, 50)
    M25 = R
    Exit Function
EH:
    M25 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M26() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim s As String
    s = "na" & ChrW(239) & "ve"
    R = lstrlenA(s) & "|" & lstrlenA("na" & ChrW(239) & "ve") & "|" & MulDiv(65536, 65536, 0) & "|" & Sgn(lstrcmpA("na" & ChrW(239) & "ve", "caf" & ChrW(233))) & Sgn(lstrcmpiA("na" & ChrW(239) & "ve", "caf" & ChrW(233))) & "|" & GetACP() & "|" & IsCharAlphaA(127) & IsCharUpperA(127) & IsCharLowerA(127) & "|" & AbsC(65536) & "|" & Err.Number
    M26 = R
    Exit Function
EH:
    M26 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M27() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, a() As R9Rec, i As Long
    ReDim a(1 To 1)
    For i = 1 To 1
        SetRec a(i), i * 3, "n" & i
    Next i
    SetRec rc, 9, "nine"
    rc = a(1): rc.Id = rc.Id + 1: a(1) = rc
    R = RecArrStr(a) & "|" & RecStr(rc) & "|" & LBound(a) & UBound(a)
    M27 = R
    Exit Function
EH:
    M27 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M28() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R9Src, k1 As R9Sink, k2 As R9Sink, v1 As String, v2 As String, a As Long, b As Variant
    Set s = New R9Src: Set k1 = New R9Sink: Set k2 = New R9Sink
    s.Tag = "s": k1.Id = "1": k2.Id = "2"
    k1.Mode = 0: k2.Mode = 4
    Set k1.Src = s
    ThisWorkbook.Attach s
    R9Log = ""
    a = -1: b = Empty
    On Error Resume Next
    v1 = s.Fire1(-1)
    R = v1 & "|" & Err.Number & "|" & R9Log
    Err.Clear
    v2 = s.Fire2(a, b)
    R = R & "|" & v2 & "|" & a & Dmp(b) & "|" & Err.Number & "|" & R9Log
    Err.Clear
    s.Fire3
    R = R & "|" & Err.Number & "|" & R9Log & "|" & TypeName(k1.Src) & TypeName(k2.Src)
    Err.Clear
    Sheet1.Detach: ThisWorkbook.Detach
    Set k2 = Nothing
    R9Log = ""
    s.Fire3
    R = R & "|" & R9Log
    Err.Clear
    Set s = Nothing
    R = R & "|" & Sheet1.GetHits() & ThisWorkbook.GetHits()
    M28 = R
    Exit Function
EH:
    M28 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M29() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("VALUE(""1,5"")")
    R = DmpX(v) & "|" & Err.Number & "|" & TypeName(v)
    M29 = R
    Exit Function
EH:
    M29 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M30() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, f As R9Fix, p As R9Pt, s As SP, q As SQ, o As SOdd
    R = Len(rc) & "," & LenB(rc) & "," & Len(f) & "," & LenB(f) & "," & Len(p) & "," & LenB(p) & "," & Len(s) & "," & LenB(s) & "," & Len(q) & "," & LenB(q) & "," & Len(o) & "," & LenB(o) & "," & Len(rc.Pt) & "," & LenB(rc.Nm) & "," & Len(rc.Nm) & "," & LenB(q.P)
    M30 = R
    Exit Function
EH:
    M30 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M31() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    p = MkPt(2, -3, "s")
    Call BumpPt(p)
    R = PtStr(p) & "|" & PtStr(q)
    M31 = R
    Exit Function
EH:
    M31 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M32() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\v01_97532.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    p = MkPt(13, 2.5, "abcdef")
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , p
    s = LOF(f) & "|"
    Seek #f, 1
    Get #f, , p2
    s = s & Loc(f) & "|" & PtStr(p2)
    Close #f
    Kill fn
    R = s
    M32 = R
    Exit Function
EH:
    M32 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M33() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, v As Variant
    a = Array("b","a","c")
    On Error Resume Next
    v = Application.WorksheetFunction.Min(a)
    R = DmpX(v) & "|" & Err.Number & "|" & Left(Err.Description, 40)
    M33 = R
    Exit Function
EH:
    M33 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M34() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    p = MkPt(1, -3, "abc")
    Call BumpPt(p)
    R = PtStr(p) & "|" & PtStr(q)
    M34 = R
    Exit Function
EH:
    M34 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M35() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 3
        GoSub Body
    Next i
    GoTo Fin
Body:
    Select Case i
        Case 1: s = s & "one"
        Case 2: GoSub Inner: s = s & "two"
        Case Else: s = s & "other"
    End Select
    Return
Inner:
    s = s & "(in" & i & ")"
    Return
Fin:
    R = s
    M35 = R
    Exit Function
EH:
    M35 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M36() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim n As Long, s As String, i As Long
    n = 0
    GoSub L1: GoSub L2: GoSub L1
    GoTo Fin
L1:
    s = s & "1" & n
    n = n + 1
    Return
L2:
    s = s & "2"
    GoSub L1
    s = s & "b"
    Return
L3:
    s = s & "3"
    If n > 2 Then Return
    s = s & "x"
    GoSub L3
    Return
Fin:
    R = s & "|" & n
    M36 = R
    Exit Function
EH:
    M36 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M37() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")")
    R = DmpX(v) & "|" & Err.Number & "|" & TypeName(v)
    M37 = R
    Exit Function
EH:
    M37 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M38() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim n As Long, s As String, i As Long
    n = 2
    For i = 1 To 5: GoSub L1: If i = 2 Then Exit For
 Next i
    GoTo Fin
L1:
    s = s & "1" & n
    n = n + 1
    Return
L2:
    s = s & "2"
    GoSub L1
    s = s & "b"
    Return
L3:
    s = s & "3"
    If n > 2 Then Return
    s = s & "x"
    GoSub L3
    Return
Fin:
    R = s & "|" & n
    M38 = R
    Exit Function
EH:
    M38 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M39() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 3
        GoSub Body
    Next i
    GoTo Fin
Body:
    Select Case i
        Case 1: s = s & "one"
        Case 2: GoSub Inner: s = s & "two"
        Case Else: s = s & "other"
    End Select
    Return
Inner:
    s = s & "(in" & i & ")"
    Return
Fin:
    R = s
    M39 = R
    Exit Function
EH:
    M39 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M40() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    Static_n = 0
    p = MkPt(7, 1, "x")
    q = p
    p.X = p.X + 1
    q.S = "q"
    R = PtStr(p) & PtStr(q) & StatUdt() & StatUdt()
    M40 = R
    Exit Function
EH:
    M40 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M41() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("TEXT(1234.5,""#,##0.00"")")
    R = DmpX(v) & "|" & Err.Number & "|" & TypeName(v)
    M41 = R
    Exit Function
EH:
    M41 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M42() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\v01_17525.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    p = MkPt(13, 2.5, "caf" & ChrW(233))
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , p
    s = LOF(f) & "|"
    Seek #f, 1
    Get #f, , p2
    s = s & Loc(f) & "|" & PtStr(p2)
    Close #f
    Kill fn
    R = s
    M42 = R
    Exit Function
EH:
    M42 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M43() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim n As Long, s As String, i As Long
    n = 3
    GoSub L1: GoSub L2: GoSub L1
    GoTo Fin
L1:
    s = s & "1" & n
    n = n + 1
    Return
L2:
    s = s & "2"
    GoSub L1
    s = s & "b"
    Return
L3:
    s = s & "3"
    If n > 2 Then Return
    s = s & "x"
    GoSub L3
    Return
Fin:
    R = s & "|" & n
    M43 = R
    Exit Function
EH:
    M43 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M44() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StatCnt() & "," & StatStr("") & "," & StatCnt() & "," & StatAll() & "," & StatStr("") & "," & StatUdt()
    M44 = R
    Exit Function
EH:
    M44 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M45() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R9Src, k1 As R9Sink, k2 As R9Sink, v1 As String, v2 As String, a As Long, b As Variant
    Set s = New R9Src: Set k1 = New R9Sink: Set k2 = New R9Sink
    s.Tag = "s": k1.Id = "1": k2.Id = "2"
    k1.Mode = 4: k2.Mode = 2
    Set k1.Src = s
    Sheet1.Attach s
    R9Log = ""
    a = 0: b = 7
    On Error Resume Next
    v1 = s.Fire1(0)
    R = v1 & "|" & Err.Number & "|" & R9Log
    Err.Clear
    v2 = s.Fire2(a, b)
    R = R & "|" & v2 & "|" & a & Dmp(b) & "|" & Err.Number & "|" & R9Log
    Err.Clear
    s.Fire3
    R = R & "|" & Err.Number & "|" & R9Log & "|" & TypeName(k1.Src) & TypeName(k2.Src)
    Err.Clear
    Sheet1.Detach: ThisWorkbook.Detach
    Set k2 = Nothing
    R9Log = ""
    s.Fire3
    R = R & "|" & R9Log
    Err.Clear
    Set s = Nothing
    R = R & "|" & Sheet1.GetHits() & ThisWorkbook.GetHits()
    M45 = R
    Exit Function
EH:
    M45 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M46() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R9Src, k1 As R9Sink, k2 As R9Sink, v1 As String, v2 As String, a As Long, b As Variant
    Set s = New R9Src: Set k1 = New R9Sink: Set k2 = New R9Sink
    s.Tag = "s": k1.Id = "1": k2.Id = "2"
    k1.Mode = 0: k2.Mode = 4
    Set k1.Src = s
    Sheet1.Attach s: ThisWorkbook.Attach s: Set k2.Src = s
    R9Log = ""
    a = 13: b = Null
    On Error Resume Next
    v1 = s.Fire1(13)
    R = v1 & "|" & Err.Number & "|" & R9Log
    Err.Clear
    v2 = s.Fire2(a, b)
    R = R & "|" & v2 & "|" & a & Dmp(b) & "|" & Err.Number & "|" & R9Log
    Err.Clear
    s.Fire3
    R = R & "|" & Err.Number & "|" & R9Log & "|" & TypeName(k1.Src) & TypeName(k2.Src)
    Err.Clear
    Sheet1.Detach: ThisWorkbook.Detach
    Set k2 = Nothing
    R9Log = ""
    s.Fire3
    R = R & "|" & R9Log
    Err.Clear
    Set s = Nothing
    R = R & "|" & Sheet1.GetHits() & ThisWorkbook.GetHits()
    M46 = R
    Exit Function
EH:
    M46 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M47() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, f As R9Fix, p As R9Pt, s As SP, q As SQ, o As SOdd
    R = Len(rc) & "," & LenB(rc) & "," & Len(f) & "," & LenB(f) & "," & Len(p) & "," & LenB(p) & "," & Len(s) & "," & LenB(s) & "," & Len(q) & "," & LenB(q) & "," & Len(o) & "," & LenB(o) & "," & Len(rc.Pt) & "," & LenB(rc.Nm) & "," & Len(rc.Nm) & "," & LenB(q.P)
    M47 = R
    Exit Function
EH:
    M47 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M48() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long
    On Error GoTo EH2
    n = 1 / 0: GoSub G1
    GoTo Fin
G1:
    s = s & "g"
    n = n + 1
    Err.Raise 9
    Return
EH2:
    s = s & "E" & Err.Number
    Resume Next
Fin:
    R = s & n
    M48 = R
    Exit Function
EH:
    M48 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M49() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String, i As Long
    s = "Hello World"
    For i = 1 To Len(s)
        t = t & AscW(Mid(s, i, 1)) & "/" & Asc(Mid(s, i, 1)) & ","
    Next i
    R = t & "|" & Hx(UCase(s)) & Hx(LCase(s)) & Hx(StrConv(s, vbProperCase)) & "|" & StrComp(s, UCase(s), vbBinaryCompare) & StrComp(s, UCase(s), vbTextCompare) & "|" & InStr(1, s, ChrW(233), vbTextCompare) & InStr(s, "E") & "|" & Hx(LeftB(s, 3)) & Hx(MidB(s, 2, 2)) & Hx(RightB(s, 1)) & InStrB(s, "c")
    M49 = R
    Exit Function
EH:
    M49 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M50() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("TYPE({1,2})")
    R = DmpX(v) & "|" & Err.Number & "|" & TypeName(v)
    M50 = R
    Exit Function
EH:
    M50 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M51() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long
    On Error GoTo EH2
    Err.Raise 6: GoSub G1
    GoTo Fin
G1:
    s = s & "g"
    n = n + 1
    Err.Raise 9
    Return
EH2:
    s = s & "E" & Err.Number
    Resume Next
Fin:
    R = s & n
    M51 = R
    Exit Function
EH:
    M51 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M52() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, i As Long
    For i = 1 To 2
        c.Grow i + 1
    Next i
    R = c.DynStr()
    M52 = R
    Exit Function
EH:
    M52 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M53() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\v01_18871.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    p = MkPt(12, 2.5, "")
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , p
    s = LOF(f) & "|"
    Seek #f, 1
    Get #f, , p2
    s = s & Loc(f) & "|" & PtStr(p2)
    Close #f
    Kill fn
    R = s
    M53 = R
    Exit Function
EH:
    M53 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M54() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    p = MkPt(0, 0.5, "s")
    ByRefL p.X
    R = PtStr(p) & "|" & PtStr(q)
    M54 = R
    Exit Function
EH:
    M54 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M55() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As SP, q As SQ, i As Long
    s.A = 5: s.B = 12: s.C(1) = 1: s.C(2) = 32767
    q.P = s: q.Q = 1.5: q.V = CDec(3)
    q.P.A = 9: s = q.P
    R = s.A & Hx(s.B) & s.C(1) & s.C(2) & "|" & q.P.A & Hx(q.P.B) & q.P.C(2) & "|" & q.Q & Dmp(q.V)
    M55 = R
    Exit Function
EH:
    M55 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M56() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String
    s = "A" & ChrW(255) & "z"
    t = StrConv(s, vbFromUnicode)
    R = Len(t) & "|" & Hx(t) & "|" & Hx(StrConv(t, vbUnicode)) & "|" & Hx(Format(s)) & "|" & lstrlenA(s) & "|" & Len(StrConv(s, vbFromUnicode, 1033)) & Hx(StrConv(s, vbUpperCase)) & Hx(StrConv(s, vbLowerCase))
    M56 = R
    Exit Function
EH:
    M56 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M57() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    Static_n = 0
    p = MkPt(13, 1, "x")
    q = p
    p.X = p.X + 1
    q.S = "q"
    R = PtStr(p) & PtStr(q) & StatUdt() & StatUdt()
    M57 = R
    Exit Function
EH:
    M57 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M58() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, a() As R9Rec, i As Long
    ReDim a(1 To 2)
    For i = 1 To 2
        SetRec a(i), i * 1, "n" & i
    Next i
    SetRec rc, 9, "nine"
    ReDim Preserve a(1 To 4): a(4) = rc
    R = RecArrStr(a) & "|" & RecStr(rc) & "|" & LBound(a) & UBound(a)
    M58 = R
    Exit Function
EH:
    M58 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M59() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\v01_65090.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    p = MkPt(5, 2.5, "abcdef")
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , p
    s = LOF(f) & "|"
    Seek #f, 1
    Get #f, , p2
    s = s & Loc(f) & "|" & PtStr(p2)
    Close #f
    Kill fn
    R = s
    M59 = R
    Exit Function
EH:
    M59 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M60() As Variant
    On Error GoTo EH
    Dim R As Variant
    R9Log = ""
    Set m_keepSrc = New R9Src
    m_keepSrc.Tag = "m"
    Sheet1.Attach m_keepSrc
    ThisWorkbook.Attach m_keepSrc
    R = m_keepSrc.Fire1(13) & "|" & Sheet1.GetHits() & "|" & ThisWorkbook.GetHits()
    m_keepSrc.Fire3
    R = R & "|" & Sheet1.GetHits() & ThisWorkbook.GetHits() & R9Log
    Set m_keepSrc = Nothing
    M60 = R
    Exit Function
EH:
    M60 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M61() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, b() As Byte, i As Long, t As String
    s = "na" & ChrW(239) & "ve"
    b = StrConv(s, vbFromUnicode)
    For i = LBound(b) To UBound(b)
        t = t & Hex(b(i)) & "."
    Next i
    R = t & "|" & LenB(s) & "|" & LenB(StrConv(s, vbFromUnicode)) & "|" & Len(StrConv(s, vbFromUnicode)) & "|" & Hx(StrConv(StrConv(s, vbFromUnicode), vbUnicode)) & "|" & Hx(StrConv(s, vbUnicode))
    M61 = R
    Exit Function
EH:
    M61 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M62() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    Static_n = 0
    p = MkPt(13, 1, "x")
    q = p
    p.X = p.X + 1
    q.S = "q"
    R = PtStr(p) & PtStr(q) & StatUdt() & StatUdt()
    M62 = R
    Exit Function
EH:
    M62 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M63() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\v01_63079.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    p = MkPt(0, 2.5, "caf" & ChrW(233))
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , p
    s = LOF(f) & "|"
    Seek #f, 1
    Get #f, , p2
    s = s & Loc(f) & "|" & PtStr(p2)
    Close #f
    Kill fn
    R = s
    M63 = R
    Exit Function
EH:
    M63 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M64() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("COLUMN(C3)")
    R = DmpX(v) & "|" & Err.Number & "|" & TypeName(v)
    M64 = R
    Exit Function
EH:
    M64 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M65() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Scope, o As Object, s As String
    On Error Resume Next
    s = CallByName(c, "FN", VbLet, 3)
    R = s & "|" & Err.Number & "|" & c.Cnt()
    M65 = R
    Exit Function
EH:
    M65 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M66() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, f As R9Fix, p As R9Pt, s As SP, q As SQ, o As SOdd
    R = Len(rc) & "," & LenB(rc) & "," & Len(f) & "," & LenB(f) & "," & Len(p) & "," & LenB(p) & "," & Len(s) & "," & LenB(s) & "," & Len(q) & "," & LenB(q) & "," & Len(o) & "," & LenB(o) & "," & Len(rc.Pt) & "," & LenB(rc.Nm) & "," & Len(rc.Nm) & "," & LenB(q.P)
    M66 = R
    Exit Function
EH:
    M66 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M67() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    R = lstrlenA(ChrW(8364) & ChrW(955)) & "|" & Err.Number & "|" & MulDiv(1, 1, 1) & "|" & NoFn() & "|" & Err.Number & "|" & NoLib() & "|" & Err.Number & "|" & lstrlenA(5) & Err.Number & lstrlenA(Null) & Err.Number
    M67 = R
    Exit Function
EH:
    M67 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M68() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, f As R9Fix, p As R9Pt, s As SP, q As SQ, o As SOdd
    R = Len(rc) & "," & LenB(rc) & "," & Len(f) & "," & LenB(f) & "," & Len(p) & "," & LenB(p) & "," & Len(s) & "," & LenB(s) & "," & Len(q) & "," & LenB(q) & "," & Len(o) & "," & LenB(o) & "," & Len(rc.Pt) & "," & LenB(rc.Nm) & "," & Len(rc.Nm) & "," & LenB(q.P)
    M68 = R
    Exit Function
EH:
    M68 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M69() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, p As R9Pt, q As R9Pt
    R9Log = ""
    c.Tag = "c"
    p = MkPt(3, 3.5, "z")
    c.SetEl 1, p: c.BumpEl 1
    q = c.Pt
    R = PtStr(p) & "|" & PtStr(q) & "|" & c.ElStr(1) & "|" & c.ElStr(2) & "|" & c.LocStr() & "|" & c.WithTest() & "|" & R9Log
    M69 = R
    Exit Function
EH:
    M69 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M70() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String, i As Long
    s = "ABC"
    For i = 1 To Len(s)
        t = t & AscW(Mid(s, i, 1)) & "/" & Asc(Mid(s, i, 1)) & ","
    Next i
    R = t & "|" & Hx(UCase(s)) & Hx(LCase(s)) & Hx(StrConv(s, vbProperCase)) & "|" & StrComp(s, UCase(s), vbBinaryCompare) & StrComp(s, UCase(s), vbTextCompare) & "|" & InStr(1, s, ChrW(233), vbTextCompare) & InStr(s, "E") & "|" & Hx(LeftB(s, 3)) & Hx(MidB(s, 2, 2)) & Hx(RightB(s, 1)) & InStrB(s, "c")
    M70 = R
    Exit Function
EH:
    M70 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M71() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As SP, q As SQ, i As Long
    s.A = 5: s.B = 12: s.C(1) = 1: s.C(2) = 32767
    q.P = s: q.Q = 1.5: q.V = CDec(3)
    q.P.A = 9: s = q.P
    R = s.A & Hx(s.B) & s.C(1) & s.C(2) & "|" & q.P.A & Hx(q.P.B) & q.P.C(2) & "|" & q.Q & Dmp(q.V)
    M71 = R
    Exit Function
EH:
    M71 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M72() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String
    s = "A" & ChrW(255) & "z"
    t = StrConv(s, vbFromUnicode)
    R = Len(t) & "|" & Hx(t) & "|" & Hx(StrConv(t, vbUnicode)) & "|" & Hx(Format(s)) & "|" & lstrlenA(s) & "|" & Len(StrConv(s, vbFromUnicode, 1033)) & Hx(StrConv(s, vbUpperCase)) & Hx(StrConv(s, vbLowerCase))
    M72 = R
    Exit Function
EH:
    M72 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M73() As Variant
    On Error GoTo EH
    Dim R As Variant
    R9Log = ""
    Set m_keepSrc = New R9Src
    m_keepSrc.Tag = "m"
    Sheet1.Attach m_keepSrc
    ThisWorkbook.Attach m_keepSrc
    R = m_keepSrc.Fire1(13) & "|" & Sheet1.GetHits() & "|" & ThisWorkbook.GetHits()
    Set m_keepSrc = New R9Src: m_keepSrc.Fire3
    R = R & "|" & Sheet1.GetHits() & ThisWorkbook.GetHits() & R9Log
    Set m_keepSrc = Nothing
    M73 = R
    Exit Function
EH:
    M73 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M74() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As New R9Src, k As R9Sink, i As Long
    R9Log = ""
    For i = 1 To 2
        Set k = New R9Sink
        k.Id = CStr(i)
        Set k.Src = s
    Next i
    R9Log = R9Log & "|"
    R = s.Fire1(0) & "|" & R9Log
    M74 = R
    Exit Function
EH:
    M74 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M75() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, p As R9Pt, q As R9Pt
    R9Log = ""
    c.Tag = "c"
    p = MkPt(12, 3.5, "z")
    c.SetEl 2, p: c.BumpEl 2: c.BumpEl 2
    q = c.Pt
    R = PtStr(p) & "|" & PtStr(q) & "|" & c.ElStr(1) & "|" & c.ElStr(2) & "|" & c.LocStr() & "|" & c.WithTest() & "|" & R9Log
    M75 = R
    Exit Function
EH:
    M75 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M76() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, b() As Byte, i As Long, t As String
    s = ""
    b = StrConv(s, vbFromUnicode)
    For i = LBound(b) To UBound(b)
        t = t & Hex(b(i)) & "."
    Next i
    R = t & "|" & LenB(s) & "|" & LenB(StrConv(s, vbFromUnicode)) & "|" & Len(StrConv(s, vbFromUnicode)) & "|" & Hx(StrConv(StrConv(s, vbFromUnicode), vbUnicode)) & "|" & Hx(StrConv(s, vbUnicode))
    M76 = R
    Exit Function
EH:
    M76 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M77() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StatCnt() & "," & StatStr("a") & "," & StatCnt() & "," & StatAll() & "," & StatStr("a") & "," & StatUdt()
    M77 = R
    Exit Function
EH:
    M77 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M78() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, v As Variant
    v = c
    c.Tag = "V"
    R = TypeName(v) & v.Tag & v.LocStr() & IsObject(v)
    v.Grow 2
    R = R & v.DynStr()
    M78 = R
    Exit Function
EH:
    M78 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M79() As Variant
    On Error GoTo EH
    Dim R As Variant
    R9Log = ""
    Set m_keepSrc = New R9Src
    m_keepSrc.Tag = "m"
    Sheet1.Attach m_keepSrc
    ThisWorkbook.Attach m_keepSrc
    R = m_keepSrc.Fire1(2) & "|" & Sheet1.GetHits() & "|" & ThisWorkbook.GetHits()
    ThisWorkbook.Detach: m_keepSrc.Fire3
    R = R & "|" & Sheet1.GetHits() & ThisWorkbook.GetHits() & R9Log
    Set m_keepSrc = Nothing
    M79 = R
    Exit Function
EH:
    M79 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M80() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("1/3")
    R = DmpX(v) & "|" & DmpX(Application.Evaluate("1/0")) & "|" & Err.Number
    M80 = R
    Exit Function
EH:
    M80 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M81() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Scope, o As Object, s As String
    On Error Resume Next
    s = CallByName(c, "PubCallsPriv", VbMethod, 2)
    R = s & "|" & Err.Number & "|" & c.Cnt()
    M81 = R
    Exit Function
EH:
    M81 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M82() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As New R9Src, k As R9Sink, i As Long
    R9Log = ""
    For i = 1 To 2
        Set k = New R9Sink
        k.Id = CStr(i)
        Set k.Src = s
    Next i
    R9Log = R9Log & "|"
    R = s.Fire1(0) & "|" & R9Log
    M82 = R
    Exit Function
EH:
    M82 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M83() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, c As New R9Cls
    R9Log = ""
    With rc
        .Id = 3
        With .Pt
            .X = 1
            With c
                .Tag = "t"
                .SetPt rc.Pt
            End With
            .S = "in"
        End With
        .Nm = "nm"
        .Tags(2) = .Id + .Pt.X
    End With
    Dim p As R9Pt
    p = c.Pt
    R = RecStr(rc) & "|" & PtStr(p)
    M83 = R
    Exit Function
EH:
    M83 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M84() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.ExecuteExcel4Macro("1/0")
    R = DmpX(v) & "|" & Err.Number & "|" & Err.Description
    M84 = R
    Exit Function
EH:
    M84 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M85() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 3
        GoSub Body
    Next i
    GoTo Fin
Body:
    Select Case i
        Case 1: s = s & "one"
        Case 2: GoSub Inner: s = s & "two"
        Case Else: s = s & "other"
    End Select
    Return
Inner:
    s = s & "(in" & i & ")"
    Return
Fin:
    R = s
    M85 = R
    Exit Function
EH:
    M85 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M86() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, o As Object, v As Variant, s As String
    c.SetEl 1, MkPt(4, 5, "late")
    Set o = c
    s = o.ElStr(1)
    v = c.ElStr(1)
    s = s & "|" & v & "|" & CallByName(o, "ElStr", VbMethod, 1) & "|" & TypeName(o.Tag)
    c.Tag = "T"
    s = s & CallByName(o, "Tag", VbGet) & CallByName(c, "LocStr", VbMethod)
    R = s
    M86 = R
    Exit Function
EH:
    M86 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M87() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Scope, o As Object, s As String
    On Error Resume Next
    Set o = c: s = o(2) & o.Item(1)
    R = s & "|" & Err.Number & "|" & c.Cnt()
    M87 = R
    Exit Function
EH:
    M87 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M88() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec
    rc.Nm = "a"
    Mid(rc.Nm, 2, 2) = "QQQ"
    R = Hx(rc.Nm) & Len(rc.Nm) & LenB(rc.Nm) & "|" & Hx(rc2.Nm) & "|" & (rc.Nm = rc2.Nm) & (rc.Nm = "ab")
    M88 = R
    Exit Function
EH:
    M88 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M89() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    Static_n = 0
    p = MkPt(0, 1, "x")
    q = p
    p.X = p.X + 1
    q.S = "q"
    R = PtStr(p) & PtStr(q) & StatUdt() & StatUdt()
    M89 = R
    Exit Function
EH:
    M89 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M90() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String, n As Long
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\u01_30853.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    SetRec rc, 13, "ab" & 13
    p = MkPt(13, 1.5, "var")
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , rc
    n = LOF(f)
    Seek #f, 1
    Get #f, , rc2
    s = RecStr(rc2) & "|" & Loc(f) & "|" & LOF(f) & "|" & EOF(f)
    Close #f
    Kill fn
    R = s & "|" & PtStr(p2) & "|" & n
    M90 = R
    Exit Function
EH:
    M90 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M91() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String, i As Long
    For i = 126 To 126 + 5
        t = t & Hx(Chr(i)) & AscW(Chr(i)) & "/" & Asc(ChrW(i)) & ","
    Next i
    R = t
    M91 = R
    Exit Function
EH:
    M91 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M92() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, p As R9Pt
    c.SetEl 2, MkPt(13, 1.5, "e")
    With c
        .Tag = "w"
        .BumpEl 2
        R = .WithTest() & .ElStr(2)
        With .Pt
            R = R & "."
        End With
    End With
    R = R & c.Tag
    M92 = R
    Exit Function
EH:
    M92 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M93() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, v As Variant, w As Variant
    a = Array(Array(1,2),3)
    On Error Resume Next
    v = Application.Product(a)
    R = DmpX(v) & "|" & Err.Number
    Err.Clear
    w = Application.WorksheetFunction.Product(a)
    R = R & "|" & DmpX(w) & "|" & Err.Number & "|" & Left(Err.Description, 50)
    M93 = R
    Exit Function
EH:
    M93 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M94() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As New R9Src, k As R9Sink, i As Long
    R9Log = ""
    For i = 1 To 2
        Set k = New R9Sink
        k.Id = CStr(i)
        Set k.Src = s
    Next i
    R9Log = R9Log & "|"
    R = s.Fire1(0) & "|" & R9Log
    M94 = R
    Exit Function
EH:
    M94 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M95() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    p = MkPt(2, 0.5, "s")
    q = MkPt(9, 9, "q")
    R = PtStr(p) & "|" & PtStr(q)
    M95 = R
    Exit Function
EH:
    M95 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M96() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, b() As Byte, i As Long, t As String
    s = ""
    b = s
    For i = LBound(b) To UBound(b)
        t = t & Hex(b(i)) & "."
    Next i
    R = t & "|" & Hx(b) & "|" & UBound(b) & "|" & Hx(StrConv(b, vbUnicode)) & "|" & Hx(StrConv(b, vbFromUnicode)) & "|" & Hx(CStr(b))
    M96 = R
    Exit Function
EH:
    M96 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M97() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, p As R9Pt, q As R9Pt
    R9Log = ""
    c.Tag = "c"
    p = MkPt(2, 3.5, "z")
    c.SetPt p: c.BumpIt p: p = c.GetPt()
    q = c.Pt
    R = PtStr(p) & "|" & PtStr(q) & "|" & c.ElStr(1) & "|" & c.ElStr(2) & "|" & c.LocStr() & "|" & c.WithTest() & "|" & R9Log
    M97 = R
    Exit Function
EH:
    M97 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M98() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("TRUE")
    R = DmpX(v) & "|" & DmpX(Application.Evaluate("""x""+1")) & "|" & Err.Number
    M98 = R
    Exit Function
EH:
    M98 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M99() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim fx As R9Fix, rw As R9Raw, bt As R9Bytes, i As Long, s As String
    fx.A = 65: fx.B = "abcd": fx.C = 305419896
    LSet rw = fx
    For i = 0 To 8
        s = s & Hex(bt.Bt(i)) & "."
    Next i
    R = Hx(rw.Raw) & "|" & s & "|" & fx.A & Hx(fx.B) & fx.C & "|" & Len(fx) & "," & LenB(fx) & "," & Len(rw) & "," & LenB(rw) & "," & Len(bt) & "," & LenB(bt)
    M99 = R
    Exit Function
EH:
    M99 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M100() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String, n As Long
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\u01_67505.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    SetRec rc, 1, "ab" & 1
    p = MkPt(1, 1.5, "var")
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , rc
    n = LOF(f)
    Seek #f, 1
    Get #f, , rc2
    s = RecStr(rc2) & "|" & Loc(f) & "|" & LOF(f) & "|" & EOF(f)
    Close #f
    Kill fn
    R = s & "|" & PtStr(p2) & "|" & n
    M100 = R
    Exit Function
EH:
    M100 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M101() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, v As Variant
    a = Array(1.5,2.5,-1)
    On Error Resume Next
    v = Application.WorksheetFunction.Max(a)
    R = DmpX(v) & "|" & Err.Number & "|" & Left(Err.Description, 40)
    M101 = R
    Exit Function
EH:
    M101 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M102() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R9Src, k1 As R9Sink, k2 As R9Sink, v1 As String, v2 As String, a As Long, b As Variant
    Set s = New R9Src: Set k1 = New R9Sink: Set k2 = New R9Sink
    s.Tag = "s": k1.Id = "1": k2.Id = "2"
    k1.Mode = 2: k2.Mode = 0
    Set k1.Src = s
    ThisWorkbook.Attach s
    R9Log = ""
    a = 5: b = 1.5
    On Error Resume Next
    v1 = s.Fire1(5)
    R = v1 & "|" & Err.Number & "|" & R9Log
    Err.Clear
    v2 = s.Fire2(a, b)
    R = R & "|" & v2 & "|" & a & Dmp(b) & "|" & Err.Number & "|" & R9Log
    Err.Clear
    s.Fire3
    R = R & "|" & Err.Number & "|" & R9Log & "|" & TypeName(k1.Src) & TypeName(k2.Src)
    Err.Clear
    Sheet1.Detach: ThisWorkbook.Detach
    Set k2 = Nothing
    R9Log = ""
    s.Fire3
    R = R & "|" & R9Log
    Err.Clear
    Set s = Nothing
    R = R & "|" & Sheet1.GetHits() & ThisWorkbook.GetHits()
    M102 = R
    Exit Function
EH:
    M102 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M103() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, i As Long, s As String
    For i = 1 To 3
        rc.Tags(i) = i * 50
    Next i
    rc2 = rc
    For i = LBound(rc2.Tags) To UBound(rc2.Tags)
        s = s & rc2.Tags(i) & ","
    Next i
    R = s & RecStr(rc)
    M103 = R
    Exit Function
EH:
    M103 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M104() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, f As Integer, fn As String, s As String
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\w01_22684.dat"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    f = FreeFile
    Open fn For Random As #f Len = Len(rc)
    SetRec rc, 1, "one"
    Put #f, 1, rc
    SetRec rc, 2, "two"
    Put #f, 5, rc
    s = LOF(f) & "|" & Len(rc) & "|"
    Get #f, 1, rc2
    s = s & RecStr(rc2) & "|" & Loc(f) & "|" & EOF(f)
    Close #f
    Kill fn
    R = s
    M104 = R
    Exit Function
EH:
    M104 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M105() As Variant
    On Error GoTo EH
    Dim R As Variant
    R9Log = ""
    Set m_keepSrc = New R9Src
    m_keepSrc.Tag = "m"
    Sheet1.Attach m_keepSrc
    ThisWorkbook.Attach m_keepSrc
    R = m_keepSrc.Fire1(1) & "|" & Sheet1.GetHits() & "|" & ThisWorkbook.GetHits()
    ThisWorkbook.Detach: m_keepSrc.Fire3
    R = R & "|" & Sheet1.GetHits() & ThisWorkbook.GetHits() & R9Log
    Set m_keepSrc = Nothing
    M105 = R
    Exit Function
EH:
    M105 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M106() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, t As String
    s = "A" & ChrW(233) & "z"
    t = StrConv(s, vbFromUnicode)
    R = Len(t) & "|" & Hx(t) & "|" & Hx(StrConv(t, vbUnicode)) & "|" & Hx(Format(s)) & "|" & lstrlenA(s) & "|" & Len(StrConv(s, vbFromUnicode, 1033)) & Hx(StrConv(s, vbUpperCase)) & Hx(StrConv(s, vbLowerCase))
    M106 = R
    Exit Function
EH:
    M106 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M107() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As R9Src, k As R9Sink
    Set s = New R9Src: s.Tag = "1"
    Set k = New R9Sink: k.Id = "k"
    R9Log = ""
    Set k.Src = s
    Set s = New R9Src: s.Tag = "2"
    R = R9Log & "|"
    Set k.Src = s
    R = R & s.Fire1(5) & "|"
    Set k = Nothing
    R = R & s.Fire1(5) & "|" & R9Log
    Set s = Nothing
    R = R & "|" & R9Log
    M107 = R
    Exit Function
EH:
    M107 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M108() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim s As String
    s = "q" & ChrW(8217) & "s"
    R = lstrlenA(s) & "|" & lstrlenA("q" & ChrW(8217) & "s") & "|" & MulDiv(0, 65536, 3) & "|" & Sgn(lstrcmpA("q" & ChrW(8217) & "s", ChrW(169) & ChrW(174))) & Sgn(lstrcmpiA("q" & ChrW(8217) & "s", ChrW(169) & ChrW(174))) & "|" & GetACP() & "|" & IsCharAlphaA(97) & IsCharUpperA(97) & IsCharLowerA(97) & "|" & AbsC(0) & "|" & Err.Number
    M108 = R
    Exit Function
EH:
    M108 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M109() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, o As Object, v As Variant, s As String
    c.SetEl 1, MkPt(4, 5, "late")
    Set o = c
    s = o.ElStr(1)
    v = c.ElStr(1)
    s = s & "|" & v & "|" & CallByName(o, "ElStr", VbMethod, 1) & "|" & TypeName(o.Tag)
    c.Tag = "T"
    s = s & CallByName(o, "Tag", VbGet) & CallByName(c, "LocStr", VbMethod)
    R = s
    M109 = R
    Exit Function
EH:
    M109 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M110() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, v As Variant
    v = c
    c.Tag = "V"
    R = TypeName(v) & v.Tag & v.LocStr() & IsObject(v)
    v.Grow 2
    R = R & v.DynStr()
    M110 = R
    Exit Function
EH:
    M110 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M111() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    R = lstrlenA(CStr(Date)) & "|" & Err.Number & "|" & MulDiv(1, 1, 1) & "|" & NoFn() & "|" & Err.Number & "|" & NoLib() & "|" & Err.Number & "|" & lstrlenA(5) & Err.Number & lstrlenA(Null) & Err.Number
    M111 = R
    Exit Function
EH:
    M111 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M112() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Scope, o As Object, s As String
    On Error Resume Next
    s = CallByName(c, "PubCallsPriv", VbMethod, 2)
    R = s & "|" & Err.Number & "|" & c.Cnt()
    M112 = R
    Exit Function
EH:
    M112 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M113() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, b() As Byte, i As Long, t As String
    s = "abc"
    b = StrConv(s, vbFromUnicode)
    For i = LBound(b) To UBound(b)
        t = t & Hex(b(i)) & "."
    Next i
    R = t & "|" & LenB(s) & "|" & LenB(StrConv(s, vbFromUnicode)) & "|" & Len(StrConv(s, vbFromUnicode)) & "|" & Hx(StrConv(StrConv(s, vbFromUnicode), vbUnicode)) & "|" & Hx(StrConv(s, vbUnicode))
    M113 = R
    Exit Function
EH:
    M113 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M114() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New R9Cls, o As Object, v As Variant, s As String
    c.SetEl 1, MkPt(4, 5, "late")
    Set o = c
    s = o.ElStr(1)
    v = c.ElStr(1)
    s = s & "|" & v & "|" & CallByName(o, "ElStr", VbMethod, 1) & "|" & TypeName(o.Tag)
    c.Tag = "T"
    s = s & CallByName(o, "Tag", VbGet) & CallByName(c, "LocStr", VbMethod)
    R = s
    M114 = R
    Exit Function
EH:
    M114 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M115() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim v As Variant
    v = Application.Evaluate("1+")
    R = DmpX(v) & "|" & Err.Number & "|" & TypeName(v)
    M115 = R
    Exit Function
EH:
    M115 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M116() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim s As String
    s = "caf" & ChrW(233)
    R = lstrlenA(s) & "|" & lstrlenA("caf" & ChrW(233)) & "|" & MulDiv(65536, 1, 1000) & "|" & Sgn(lstrcmpA("caf" & ChrW(233), "a" & ChrW(8364) & "b")) & Sgn(lstrcmpiA("caf" & ChrW(233), "a" & ChrW(8364) & "b")) & "|" & GetACP() & "|" & IsCharAlphaA(97) & IsCharUpperA(97) & IsCharLowerA(97) & "|" & AbsC(65536) & "|" & Err.Number
    M116 = R
    Exit Function
EH:
    M116 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M117() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim rc As R9Rec, rc2 As R9Rec, p As R9Pt, p2 As R9Pt, f As Integer, fn As String, s As String, n As Long
    fn = Environ$("TEMP") & "\xlio9"
    On Error Resume Next
    MkDir fn
    On Error GoTo 0
    fn = fn & "\u01_47561.bin"
    On Error Resume Next
    Kill fn
    On Error GoTo 0
    SetRec rc, 3, "ab" & 3
    p = MkPt(3, 1.5, "var")
    f = FreeFile
    Open fn For Binary As #f
    Put #f, , rc
    n = LOF(f)
    Seek #f, 1
    Get #f, , rc2
    s = RecStr(rc2) & "|" & Loc(f) & "|" & LOF(f) & "|" & EOF(f)
    Close #f
    Kill fn
    R = s & "|" & PtStr(p2) & "|" & n
    M117 = R
    Exit Function
EH:
    M117 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M118() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    R = lstrlenA(ChrW(8364) & ChrW(955)) & "|" & Err.Number & "|" & MulDiv(1, 1, 1) & "|" & NoFn() & "|" & Err.Number & "|" & NoLib() & "|" & Err.Number & "|" & lstrlenA(5) & Err.Number & lstrlenA(Null) & Err.Number
    M118 = R
    Exit Function
EH:
    M118 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M119() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As R9Pt, q As R9Pt
    p = MkPt(2, -3, "s")
    ByRefL p.X
    R = PtStr(p) & "|" & PtStr(q)
    M119 = R
    Exit Function
EH:
    M119 = "#ERR " & Err.Number & " " & Err.Description
End Function
