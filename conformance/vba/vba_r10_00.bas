Attribute VB_Name = "VbaR10_00"
Option Explicit
Private Function Z0() As Long
    Z0 = 0
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
Private Function ErrS() As String
    ErrS = Err.Number & "/" & Err.Source & "/" & Err.Description
End Function
Private Function Try(ByVal what As Long, v As Variant) As String
    On Error GoTo EH
    Select Case what
    Case 0: Try = Dmp(CInt(v))
    Case 1: Try = Dmp(CLng(v))
    Case 2: Try = Dmp(CByte(v))
    Case 3: Try = Dmp(CCur(v))
    Case 4: Try = Dmp(CDec(v))
    Case 5: Try = Dmp(Int(v))
    Case 6: Try = Dmp(Fix(v))
    Case 7: Try = Dmp(Round(v))
    Case 8: Try = Dmp(CDbl(v))
    Case 9: Try = Dmp(CStr(v))
    Case 10: Try = Dmp(CBool(v))
    Case 11: Try = Dmp(CSng(v))
    Case 12: Try = Dmp(Abs(v))
    Case 13: Try = Dmp(Sgn(v))
    End Select
    Exit Function
EH:
    Try = "#" & Err.Number
End Function
Private Sub ChainC(ByVal mode As Long)
    Dim q As Long, a(1 To 2) As Long
    R10Log = R10Log & "c" & mode
    Select Case mode
    Case 1: Err.Raise 11
    Case 2: Err.Raise vbObjectError + 5, "src", "custom text"
    Case 3: q = 1 / Z0()
    Case 4: Err.Raise 5
    Case 5: a(3) = 1
    End Select
    R10Log = R10Log & "C"
End Sub
Private Sub ChainB(ByVal mode As Long, ByVal h As Long)
    R10Log = R10Log & "b" & h
    Select Case h
    Case 0
        ChainC mode
    Case 1
        On Error Resume Next
        ChainC mode
        R10Log = R10Log & "(" & Err.Number & ")"
    Case 2
        On Error GoTo EH
        ChainC mode
        R10Log = R10Log & "B"
        Exit Sub
EH:
        R10Log = R10Log & "[eh" & Err.Number & "]"
        Resume Next
    Case 3
        On Error GoTo EH3
        ChainC mode
        R10Log = R10Log & "B"
        Exit Sub
EH3:
        R10Log = R10Log & "[eh3:" & Err.Number & "]"
        Resume Fin3
Fin3:
        R10Log = R10Log & "F"
    Case 4
        On Error GoTo EH4
        ChainC mode
        Exit Sub
EH4:
        R10Log = R10Log & "[eh4]"
        Err.Raise 99, "inner", "from handler"
    Case 5
        On Error GoTo EH5
        ChainC mode
        R10Log = R10Log & "B"
        Exit Sub
EH5:
        R10Log = R10Log & "[eh5:" & Err.Number & "]"
        Err.Clear
        On Error GoTo 0
        Resume Next
    End Select
    R10Log = R10Log & "b."
End Sub
Private Sub ChainA(ByVal mode As Long, ByVal h2 As Long, ByVal h1 As Long)
    R10Log = R10Log & "a" & h2
    Select Case h2
    Case 0
        ChainB mode, h1
    Case 1
        On Error Resume Next
        ChainB mode, h1
        R10Log = R10Log & "(" & Err.Number & ")"
    Case 2
        On Error GoTo EHA
        ChainB mode, h1
        R10Log = R10Log & "A"
        Exit Sub
EHA:
        R10Log = R10Log & "[eha" & Err.Number & ":" & Err.Source & ":" & Err.Description & "]"
        Resume Next
    End Select
    R10Log = R10Log & "a."
End Sub
Private Function Cnt3() As Long
    Static n As Long
    n = n + 1
    Cnt3 = n
End Function
Public Function RunEcho(ParamArray a() As Variant) As String
    Dim i As Long
    RunEcho = UBound(a) - LBound(a) + 1 & ":"
    For i = LBound(a) To UBound(a)
        RunEcho = RunEcho & TypeName(a(i)) & "=" & a(i) & ","
    Next i
End Function
Public Function RunArr(ByVal n As Long) As Variant
    Dim v() As Variant, i As Long
    ReDim v(1 To n)
    For i = 1 To n: v(i) = i * 1.5: Next i
    RunArr = v
End Function
Public Sub RunErrSub(ByVal n As Long)
    Err.Raise n, "RunErrSub", "boom" & n
End Sub
Public Function RunByRef(ByRef n As Long) As Long
    n = n + 5
    RunByRef = n
End Function
Private Function Sandbox() As String
    Dim d As String
    d = Environ$("TEMP") & "\xlio10"
    On Error Resume Next
    MkDir d
    d = d & "\u00_" & Cnt3()
    Wipe d
    MkDir d
    On Error GoTo 0
    Sandbox = d
End Function
Private Sub Wipe(ByVal d As String)
    Dim f As String, n As Long, i As Long, nm(1 To 60) As String
    On Error Resume Next
    f = Dir(d & "\*", vbHidden + vbReadOnly + vbSystem + vbDirectory)
    Do While Len(f) > 0 And n < 60
        If f <> "." And f <> ".." Then n = n + 1: nm(n) = f
        f = Dir()
    Loop
    For i = 1 To n
        SetAttr d & "\" & nm(i), vbNormal
        If (GetAttr(d & "\" & nm(i)) And vbDirectory) <> 0 Then RmDir d & "\" & nm(i) Else Kill d & "\" & nm(i)
    Next i
    Close
    RmDir d
End Sub
Private Function Me_() As Object
    Set Me_ = Sheet1
End Function
Private Function Missing_() As Variant
End Function
Public Function VbTen0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k \ 50
    Case 0: VbTen0 = MC0(k)
    Case 1: VbTen0 = MC1(k)
    Case 2: VbTen0 = MC2(k)
    End Select
    R10Reset
    Exit Function
EH:
    VbTen0 = "#ERR " & Err.Number & " " & Err.Description
    R10Reset
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
    Dim s As String
    R10Log = "": R10Mode = 1
    On Error Resume Next
    Sheet1.Range("A1,C3,E5:F6").Value = 1.5
    s = R10Log & "|" & Err.Number & "|" & Application.EnableEvents & Application.ScreenUpdating & Application.Calculation
    Err.Clear
    R10Reset
    R = s & "|" & Sheet1.Range("A1").Value & Sheet1.Range("B2").Value & Sheet1.Range("F1").Value & Sheet1.Range("A2").Value
    R10Log = ""
    Sheet1.Cells.ClearContents
Done:
    M0 = R
    Exit Function
EH:
    M0 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M1() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error GoTo H
    s = "a"
    ChainB 5, 0
    s = s & "z"
    R = s & R10Log
    R10Log = ""
    GoTo Done
H:
    s = s & "[H" & Err.Number & ":" & Err.Source & "]"
    Resume Next
Done:
    M1 = R
    Exit Function
EH:
    M1 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M2() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet2.Activate: Sheet2.Range("C3").Select
    s = R10Log & "|" & Err.Number & "|" & ActiveSheet.Name & "|" & ActiveCell.Address(False, False) & "|" & Selection.Address(False, False)
    Err.Clear
    Sheet1.Activate
    Sheet1.Range("A1").Select
    R10Reset
    R10Log = ""
    R = s
Done:
    M2 = R
    Exit Function
EH:
    M2 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M3() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, l As String, y As Long
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\t.txt" For Output As #f
    Print #f, "line one"
    Print #f, "a", "b"; "c"
    Print #f, 12; -3.5; True
    Print #f, Tab(5); "tab"; Spc(3); "spc"
    Print #f, Empty;
    Write #f, "q", 5, 2.5, True, Null, #1/2/2000#
    Write #f, "x,y", """quoted"""
    s = LOF(f) & "," & Loc(f) & "," & EOF(f)
    Close #f
    s = s & "|" & FileLen(d & "\t.txt") & "|" & Err.Number
    f = FreeFile
    Open d & "\t.txt" For Input As #f
    Do While Not EOF(f) And y < 12
        Line Input #f, l
        s = s & "[" & l & "]"
        y = y + 1
    Loop
    s = s & EOF(f) & "," & Loc(f) & "," & LOF(f)
    Close #f
    s = s & "|" & Err.Number
    Wipe d
    R = s
Done:
    M3 = R
    Exit Function
EH:
    M3 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M4() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet2.Select
    s = R10Log & "|" & Err.Number & "|" & ActiveSheet.Name & "|" & ActiveCell.Address(False, False) & "|" & Selection.Address(False, False)
    Err.Clear
    Sheet1.Activate
    Sheet1.Range("A1").Select
    R10Reset
    R10Log = ""
    R = s
Done:
    M4 = R
    Exit Function
EH:
    M4 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M5() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(0, 1, -1, 0.5, 1234.5678, -1234.5678, 1E10, 0.000012345, 1E-10, 123456789012)
        s = s & Hx(Format$(v, "Medium Date")) & ";"
    Next v
    R = s
Done:
    M5 = R
    Exit Function
EH:
    M5 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M6() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(2147483647.5, 2147483648, -2147483648.5, -2147483649, 2147483646.5)
        s = s & Try(13, v) & ","
    Next v
    R = s
Done:
    M6 = R
    Exit Function
EH:
    M6 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M7() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error GoTo H1
    s = "x"
    i = 1 / Z0()
    s = s & "1"
    On Error GoTo H2
    Err.Raise 7
    s = s & "2"
    On Error GoTo 0
    i = 1 / Z0()
    s = s & "3"
    R = s
    GoTo Done
H1:
    s = s & "(h1:" & Err.Number & ")"
    Resume Next
H2:
    s = s & "(h2:" & Err.Number & ")"
    Resume Next
Done:
    M7 = R
    Exit Function
EH:
    M7 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M8() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, j As Long, a(1 To 5) As String, t As String
    a(1) = "naive": a(2) = "o" & ChrW(246): a(3) = "a" & ChrW(8364) & "b": a(4) = "o" & ChrW(246): a(5) = "naive"
    For i = 1 To 4
        For j = i + 1 To 5
            If a(j) < a(i) Then t = a(i): a(i) = a(j): a(j) = t
        Next j
    Next i
    For i = 1 To 5: s = s & Hx(a(i)): Next i
    R = s
Done:
    M8 = R
    Exit Function
EH:
    M8 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M9() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Integer, l As Long, b As Byte, c As Currency, d As Variant
    On Error Resume Next
    i = -32768 * -1
    s = i & "," & Err.Number & ";"
    Err.Clear
    l = -1 * 0
    s = s & l & "," & Err.Number & ";"
    Err.Clear
    b = 0 * 1
    s = s & b & "," & Err.Number & ";"
    Err.Clear
    c = 1234.5678 * 0.0001
    s = s & c & "," & Err.Number & ";"
    Err.Clear
    d = -1 * 0
    s = s & Dmp(d) & "," & Err.Number
    R = s
Done:
    M9 = R
    Exit Function
EH:
    M9 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M10() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, l As String, y As Long
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\t.txt" For Output As #f
    Print #f, "line one"
    Print #f, "a", "b"; "c"
    Print #f, 12; -3.5; True
    Print #f, Tab(5); "tab"; Spc(3); "spc"
    Print #f, Format(5, "000")
    Write #f, "q", 5, 2.5, True, Null, #1/2/2000#
    Write #f, "x,y", """quoted"""
    s = LOF(f) & "," & Loc(f) & "," & EOF(f)
    Close #f
    s = s & "|" & FileLen(d & "\t.txt") & "|" & Err.Number
    f = FreeFile
    Open d & "\t.txt" For Input As #f
    Do While Not EOF(f) And y < 12
        Line Input #f, l
        s = s & "[" & l & "]"
        y = y + 1
    Loop
    s = s & EOF(f) & "," & Loc(f) & "," & LOF(f)
    Close #f
    s = s & "|" & Err.Number
    Wipe d
    R = s
Done:
    M10 = R
    Exit Function
EH:
    M10 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M11() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Variant, e As Variant, s As String
    d = CDec("-79228162514264337593543950335"): e = CDec(-2)
    s = Dmp(d) & Dmp(e)
    On Error Resume Next
    s = s & Dmp(d + e) & Dmp(d - e) & Dmp(d * e) & Dmp(d / e) & "|" & Err.Number
    Err.Clear
    s = s & Dmp(d \ e) & Dmp(d Mod e) & Dmp(-d) & Dmp(d ^ 2) & Dmp(Int(d)) & Dmp(Fix(d)) & Dmp(Abs(d)) & Dmp(Sgn(d)) & Dmp(Round(d, 3))
    s = s & "|" & Err.Number & "|" & (d = e) & (d < e) & "|" & CStr(d) & "|" & CDbl(d) & "|" & TypeName(d + 1) & TypeName(d + 1.5) & TypeName(d * CCur(2))
    R = s
Done:
    M11 = R
    Exit Function
EH:
    M11 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M12() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    On Error Resume Next
    For Each v In Array("3.7", "-3.7", " 4 ", "&H7F", True)
        s = s & Dmp(Int(v)) & Dmp(Fix(v)) & Err.Number & ";"
        Err.Clear
    Next v
    s = s & "|" & Int(-0#) & Fix(-0.5) & "|" & Hex(Int(-1)) & "|" & CInt(-0.5) & CInt(0.5) & CInt(1.5) & CInt(2.5) & "|" & CLng(2.5) & CLng(3.5) & CLng(-2.5)
    R = s
Done:
    M12 = R
    Exit Function
EH:
    M12 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M13() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(Join(Split("caf" & ChrW(233), "-", 2, vbBinaryCompare), "/")) & "|" & UBound(Split("caf" & ChrW(233), "-", 2, vbBinaryCompare)) & "|" & UBound(Filter(Array("caf" & ChrW(233), "o" & ChrW(246), "x"), "a", True, vbBinaryCompare))
    R = s
Done:
    M13 = R
    Exit Function
EH:
    M13 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M14() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hex(CInt(-5)) & "|" & Oct(CInt(-5)) & "|" & Hex$(1E5) & "|" & Len(Hex(CInt(-5)))
    On Error Resume Next
    s = s & "|" & Hex(2147483648#) & "|" & Err.Number & "|" & Hex(Null) & Err.Number & "|" & Oct(-1) & "|" & Hex(-32768) & Err.Number
    R = s
Done:
    M14 = R
    Exit Function
EH:
    M14 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M15() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, l As String, y As Long
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\t.txt" For Output As #f
    Print #f, "line one"
    Print #f, "a", "b"; "c"
    Print #f, 12; -3.5; True
    Print #f, Tab(5); "tab"; Spc(3); "spc"
    Print #f, Null
    Write #f, "q", 5, 2.5, True, Null, #1/2/2000#
    Write #f, "x,y", """quoted"""
    s = LOF(f) & "," & Loc(f) & "," & EOF(f)
    Close #f
    s = s & "|" & FileLen(d & "\t.txt") & "|" & Err.Number
    f = FreeFile
    Open d & "\t.txt" For Input As #f
    Do While Not EOF(f) And y < 12
        Line Input #f, l
        s = s & "[" & l & "]"
        y = y + 1
    Loop
    s = s & EOF(f) & "," & Loc(f) & "," & LOF(f)
    Close #f
    s = s & "|" & Err.Number
    Wipe d
    R = s
Done:
    M15 = R
    Exit Function
EH:
    M15 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M16() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    On Error Resume Next
    Sheet1.Range("A1").Value = 5
    Sheet1.Range("A2").Value = "t"
    v = Application.Evaluate("A1*2")
    R = DmpX(v) & "|" & Err.Number
    Sheet1.Cells.Clear
Done:
    M16 = R
    Exit Function
EH:
    M16 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M17() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Variant, e As Variant, s As String
    d = CDec(1/3): e = CDec(7)
    s = Dmp(d) & Dmp(e)
    On Error Resume Next
    s = s & Dmp(d + e) & Dmp(d - e) & Dmp(d * e) & Dmp(d / e) & "|" & Err.Number
    Err.Clear
    s = s & Dmp(d \ e) & Dmp(d Mod e) & Dmp(-d) & Dmp(d ^ 2) & Dmp(Int(d)) & Dmp(Fix(d)) & Dmp(Abs(d)) & Dmp(Sgn(d)) & Dmp(Round(d, 3))
    s = s & "|" & Err.Number & "|" & (d = e) & (d < e) & "|" & CStr(d) & "|" & CDbl(d) & "|" & TypeName(d + 1) & TypeName(d + 1.5) & TypeName(d * CCur(2))
    R = s
Done:
    M17 = R
    Exit Function
EH:
    M17 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M18() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, b() As Byte, x As Long, y As Double, z As String, w As Integer
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\r.bin" For Binary As #f
    x = 1: y = 1.5: z = "xyzzy12345": w = -2
    Put #f, , x
    Put #f, , y
    Put #f, , z
    Put #f, 21, w
    s = LOF(f) & "," & Loc(f) & "," & Seek(f) & "," & EOF(f) & "|" & Err.Number
    Seek #f, 1
    x = 0: y = 0: z = String(5, "_"): w = 0
    Get #f, , x
    Get #f, , y
    Get #f, , z
    s = s & "|" & x & "," & y & "," & Hx(z) & "," & Loc(f) & "," & Seek(f) & "," & EOF(f)
    Get #f, 21, w
    s = s & "," & w & "," & Err.Number
    Get #f, 1000, w
    s = s & "," & Err.Number & "," & EOF(f)
    Seek #f, 100
    s = s & "," & Seek(f) & "," & Loc(f) & Err.Number
    ReDim b(0 To 3)
    Get #f, 1, b
    s = s & "|" & b(0) & "." & b(1) & "." & b(2) & "." & b(3)
    Close #f
    Wipe d
    R = s
Done:
    M18 = R
    Exit Function
EH:
    M18 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M19() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 1
    On Error Resume Next
    Sheet1.Range("C3:A1").Value = 5
    s = R10Log & "|" & Err.Number & "|" & Application.EnableEvents & Application.ScreenUpdating & Application.Calculation
    Err.Clear
    R10Reset
    R = s & "|" & Sheet1.Range("A1").Value & Sheet1.Range("B2").Value & Sheet1.Range("F1").Value & Sheet1.Range("A2").Value
    R10Log = ""
    Sheet1.Cells.ClearContents
Done:
    M19 = R
    Exit Function
EH:
    M19 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M20() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = ""
    s = Application.EnableEvents & "," & Application.ScreenUpdating & "," & Application.Calculation
    Application.EnableEvents = 5
    Application.ScreenUpdating = True
    Application.Calculation = xlCalculationSemiautomatic
    s = s & "|" & Application.EnableEvents & "," & Application.ScreenUpdating & "," & Application.Calculation
    On Error Resume Next
    Application.Calculation = 99
    s = s & "|" & Err.Number & "," & Application.Calculation
    Application.EnableEvents = Null
    s = s & "|" & Err.Number & "," & TypeName(Application.EnableEvents)
    R10Reset
    R = s & "|" & Application.EnableEvents & Application.ScreenUpdating & Application.Calculation
Done:
    M20 = R
    Exit Function
EH:
    M20 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M21() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Range("A1").Value = 2
    Sheet1.Range("A2").Formula = "=A1*2"
    Sheet2.Range("A1").Formula = "=Sheet1!A2+1"
    R10Log = ""
    Application.ScreenUpdating = False: Sheet1.Range("A1").Value = 5
    s = R10Log & "|" & Err.Number & "|" & Sheet1.Range("A2").Value & "|" & Sheet2.Range("A1").Value & "|" & Application.Calculation
    Err.Clear
    R10Reset
    Application.Calculate
    s = s & "|" & Sheet2.Range("A1").Value
    R10Log = ""
    Sheet1.Cells.Clear: Sheet2.Cells.Clear
    R = s
Done:
    M21 = R
    Exit Function
EH:
    M21 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M22() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(0, 1, -1, 0.5, 1234.5678, -1234.5678, 1E10, 0.000012345, 1E-10, 123456789012)
        s = s & Hx(Format$(v, "Yes/No")) & ";"
    Next v
    R = s
Done:
    M22 = R
    Exit Function
EH:
    M22 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M23() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error GoTo H1
    s = "x"
    Err.Raise 7
    s = s & "1"
    On Error GoTo H2
    i = 1
    s = s & "2"
    On Error GoTo 0
    i = 1 / Z0()
    s = s & "3"
    R = s
    GoTo Done
H1:
    s = s & "(h1:" & Err.Number & ")"
    Resume Next
H2:
    s = s & "(h2:" & Err.Number & ")"
    Resume Next
Done:
    M23 = R
    Exit Function
EH:
    M23 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M24() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(0, 1, -1, 0.5, 1234.5678, -1234.5678, 1E10, 0.000012345, 1E-10, 123456789012)
        s = s & Hx(Format$(v, "Standard")) & ";"
    Next v
    R = s
Done:
    M24 = R
    Exit Function
EH:
    M24 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M25() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Currency, c2 As Currency, s As String, d As Double
    c = -1.23455: c2 = -0.3333
    s = c & "|" & c2 & "|" & (c + c2) & "|" & (c * c2) & "|" & TypeName(c * c2) & "|"
    On Error Resume Next
    s = s & (c / c2) & "|" & Err.Number & "|" & (c \ c2) & "|" & (c Mod c2) & "|" & Err.Number
    d = c
    s = s & "|" & d & "|" & CCur(d) & "|" & CStr(c) & "|" & Format(c, "0.0000") & "|" & CLng(c) & "|" & Int(c) & "|" & Fix(c) & "|" & Round(c, 2) & "|" & Hx(Format$(c))
    R = s
Done:
    M25 = R
    Exit Function
EH:
    M25 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M26() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(2147483647.5, 2147483648, -2147483648.5, -2147483649, 2147483646.5)
        s = s & Try(6, v) & ","
    Next v
    R = s
Done:
    M26 = R
    Exit Function
EH:
    M26 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M27() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object, v As Variant
    R10Log = ""
    Set o = New R10Src
    On Error Resume Next
    v = CallByName(o, "Nope", VbLet, 3, 4)
    R = Dmp(v) & "|" & Err.Number & ":" & Err.Description & "|" & R10Log
    R10Log = ""
Done:
    M27 = R
    Exit Function
EH:
    M27 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M28() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(Join(Split(ChrW(955) & "x", ChrW(233), 2, vbTextCompare), "/")) & "|" & UBound(Split(ChrW(955) & "x", ChrW(233), 2, vbTextCompare)) & "|" & UBound(Filter(Array(ChrW(955) & "x", "apple", "x"), "a", True, vbTextCompare))
    R = s
Done:
    M28 = R
    Exit Function
EH:
    M28 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M29() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet2.Activate: Sheet2.Range("C3").Select
    s = R10Log & "|" & Err.Number & "|" & ActiveSheet.Name & "|" & ActiveCell.Address(False, False) & "|" & Selection.Address(False, False)
    Err.Clear
    Sheet1.Activate
    Sheet1.Range("A1").Select
    R10Reset
    R10Log = ""
    R = s
Done:
    M29 = R
    Exit Function
EH:
    M29 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M30() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, a As Variant, b As Variant, c As Variant, e As Variant, l As String
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\w.txt" For Output As #f
    Write #f, "q", 5, 2.5, True, Null, "z"
    Write #f, "r", 6
    Write #f,
    Print #f, "tail,1,2"
    Close #f
    s = Err.Number & ""
    Open d & "\w.txt" For Input As #f
    Input #f, a, b, c, e
    s = s & "|" & Dmp(a) & Dmp(b) & Dmp(c) & Dmp(e)
    Input #f, a, b
    s = s & "|" & Dmp(a) & Dmp(b)
    Input #f, a
    s = s & "|" & Dmp(a) & Err.Number
    Line Input #f, l
    s = s & "|" & l
    Input #f, a, b
    s = s & "|" & Dmp(a) & Dmp(b) & Err.Number & EOF(f)
    Input #f, a
    s = s & "|" & Err.Number
    Close #f
    Wipe d
    R = s
Done:
    M30 = R
    Exit Function
EH:
    M30 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M31() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array("1e3", "&H10", "&O17", " 12 ", "1,5", "1 2", "", "-", "+5", "1d2", "$5", "5%")
        s = s & Try(6, v) & ","
    Next v
    R = s
Done:
    M31 = R
    Exit Function
EH:
    M31 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M32() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object, v As Variant
    R10Log = ""
    Set o = New R10Src
    On Error Resume Next
    v = CallByName(o, "Fire2", 0, 3, 4)
    R = Dmp(v) & "|" & Err.Number & ":" & Err.Description & "|" & R10Log
    R10Log = ""
Done:
    M32 = R
    Exit Function
EH:
    M32 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M33() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Hx(Format("abc", "On/Off")) & "|" & Hx(Format$("abc", "On/Off", vbMonday, 4)) & "|" & Err.Number
    R = s
Done:
    M33 = R
    Exit Function
EH:
    M33 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M34() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, p As Variant, t As Variant
    For Each t In Array("abc", "ABC", "a", "", "a-b", "a[b", "ab]")
        For Each p In Array("abc", "a?c", "a*", "*c", "?", "*", "", "[a-c]*", "[!a-c]*", "a[b]c")
            On Error Resume Next
            Err.Clear
            s = s & -(t Like p) & Err.Number
        Next p
        s = s & ";"
    Next t
    R = s
Done:
    M34 = R
    Exit Function
EH:
    M34 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M35() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 3
    On Error Resume Next
    Sheet1.Range("C3:A1").Value = "x"
    s = R10Log & "|" & Err.Number & "|" & Application.EnableEvents & Application.ScreenUpdating & Application.Calculation
    Err.Clear
    R10Reset
    R = s & "|" & Sheet1.Range("A1").Value & Sheet1.Range("B2").Value & Sheet1.Range("F1").Value & Sheet1.Range("A2").Value
    R10Log = ""
    Sheet1.Cells.ClearContents
Done:
    M35 = R
    Exit Function
EH:
    M35 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M36() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As String, s As String, n As Long
    d = Sandbox()
    On Error Resume Next
    MkDir d & "\sub": MkDir d & "\sub"
    f = Dir(d & "\?.txt", 99)
    Do While Len(f) > 0 And n < 12
        s = s & f & ";"
        n = n + 1
        f = Dir()
    Loop
    s = s & "|" & Err.Number
    Err.Clear
    f = Dir()
    s = s & "|" & Err.Number & "|" & Dir(d & "\nonexist.*") & "|" & Err.Number
    Wipe d
    R = s
Done:
    M36 = R
    Exit Function
EH:
    M36 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M37() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(StrConv("12", vbProperCase)) & Hx(StrConv("12", vbUpperCase)) & Hx(StrConv("12", vbLowerCase)) & "|" & Hx(Left$("12", 2)) & Hx(Right$("12", 2)) & Hx(Mid$("12", 2, 2)) & "|" & Hx(LTrim(RTrim("12"))) & Hx(Trim$("12")) & "|" & Len("12") & LenB("12") & "|" & Hx(Space(2) & "12") & Hx(Replace("12", "a", "b", , , vbTextCompare))
    R = s
Done:
    M37 = R
    Exit Function
EH:
    M37 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M38() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Hx(Format(-5, "True/False")) & "|" & Hx(Format$(-5, "True/False", 0, vbFirstFourDays)) & "|" & Err.Number
    R = s
Done:
    M38 = R
    Exit Function
EH:
    M38 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M39() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object, v As Variant
    R10Log = ""
    Set o = New R10Src
    On Error Resume Next
    v = CallByName(o, "Fire1U", VbLet, 3)
    R = Dmp(v) & "|" & Err.Number & ":" & Err.Description & "|" & R10Log
    R10Log = ""
Done:
    M39 = R
    Exit Function
EH:
    M39 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M40() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, n As Long
    n = 3
    On Error Resume Next
    v = Application.Run("RunByRef", n)
    R = Dmp(v) & n & "|" & Err.Number
    v = RunByRef(n)
    R = R & Dmp(v) & n
Done:
    M40 = R
    Exit Function
EH:
    M40 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M41() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, b() As Byte, x As Long, y As Double, z As String, w As Integer
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\r.bin" For Binary As #f
    x = -1: y = 1.5: z = "xyzzy12345": w = 5
    Put #f, , x
    Put #f, , y
    Put #f, , z
    Put #f, 1, w
    s = LOF(f) & "," & Loc(f) & "," & Seek(f) & "," & EOF(f) & "|" & Err.Number
    Seek #f, 1
    x = 0: y = 0: z = String(3, "_"): w = 0
    Get #f, , x
    Get #f, , y
    Get #f, , z
    s = s & "|" & x & "," & y & "," & Hx(z) & "," & Loc(f) & "," & Seek(f) & "," & EOF(f)
    Get #f, 1, w
    s = s & "," & w & "," & Err.Number
    Get #f, 1000, w
    s = s & "," & Err.Number & "," & EOF(f)
    Seek #f, 0
    s = s & "," & Seek(f) & "," & Loc(f) & Err.Number
    ReDim b(0 To 3)
    Get #f, 1, b
    s = s & "|" & b(0) & "." & b(1) & "." & b(2) & "." & b(3)
    Close #f
    Wipe d
    R = s
Done:
    M41 = R
    Exit Function
EH:
    M41 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M42() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim src As New R10Src, k1 As New R10Sink, k2 As New R10Sink
    R10Log = ""
    k1.Id = "1": k2.Id = "2": k1.Mode = 0: k2.Mode = 4
    k1.Attach src: k2.Attach src
    On Error Resume Next
    R = src.Fire1(1) & "|" & Err.Number
    R = R & "|" & src.Fire1U(1) & "|" & Err.Number & ":" & Err.Description
    src.Fire2
    R = R & "|" & Err.Number & "|" & R10Log
    k1.Detach
    R10Log = ""
    R = R & "|" & src.Fire1(1) & "|" & R10Log
    R10Log = ""
Done:
    M42 = R
    Exit Function
EH:
    M42 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M43() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As String, s As String, n As Long
    d = Sandbox()
    On Error Resume Next
    Open d & "\a.txt" For Output As #1: Print #1, "x": Close #1: SetAttr d & "\a.txt", vbHidden
    f = Dir(d & "\sub", vbDirectory)
    Do While Len(f) > 0 And n < 12
        s = s & f & ";"
        n = n + 1
        f = Dir()
    Loop
    s = s & "|" & Err.Number
    Err.Clear
    f = Dir()
    s = s & "|" & Err.Number & "|" & Dir(d & "\nonexist.*") & "|" & Err.Number
    Wipe d
    R = s
Done:
    M43 = R
    Exit Function
EH:
    M43 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M44() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, x As Single
    x = Rnd(-1)
    Randomize 1
    For i = 1 To 4
        s = s & Format(Rnd, "0.000000000") & ","
    Next i
    s = s & Format(Rnd(0), "0.0000000") & "," & Format(Rnd(1), "0.0000000") & "," & Format(Rnd(-3), "0.0000000") & "," & Format(Rnd, "0.0000000") & "," & Format(Rnd(2), "0.0000000")
    Randomize 1
    s = s & "|" & Format(Rnd, "0.000000000") & "|" & TypeName(Rnd) & TypeName(Rnd(3))
    R = s
Done:
    M44 = R
    Exit Function
EH:
    M44 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M45() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, k As Long
    k = 0
    On Error GoTo H
    k = k + 1
    s = s & "a" & k
    Err.Raise 5 + k
    s = s & "b"
    GoTo Done
H:
    s = s & "<" & Err.Number & ">"
    If k < 2 Then
        Resume Next
    Else
        Err.Clear
    End If
    R = s & Err.Number & "|" & k
Done:
    M45 = R
    Exit Function
EH:
    M45 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M46() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, p As Variant, t As Variant
    For Each t In Array("a1", "A1", "a 1", "1a", "a#", "a*")
        For Each p In Array("a#", "[#]", "a[-]b", "[a-]", "[]", "[!]", "a[", "[b-a]", "ab]")
            On Error Resume Next
            Err.Clear
            s = s & -(t Like p) & Err.Number
        Next p
        s = s & ";"
    Next t
    R = s
Done:
    M46 = R
    Exit Function
EH:
    M46 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M47() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, s As String, f As Integer, x As Long
    d = Sandbox()
    On Error Resume Next
    s = "x"
    Open d & "\q" For Output As #1: Get #1, , x
    s = s & "|" & Err.Number & ":" & Err.Description
    Err.Clear
    Close
    s = s & "|" & Dir(d & "\*")
    Wipe d
    R = s
Done:
    M47 = R
    Exit Function
EH:
    M47 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M48() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, n As Long
    n = 3
    On Error Resume Next
    v = Application.Run("RunByRef", n)
    R = Dmp(v) & n & "|" & Err.Number
    v = RunByRef(n)
    R = R & Dmp(v) & n
Done:
    M48 = R
    Exit Function
EH:
    M48 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M49() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    On Error Resume Next
    Sheet1.Range("A1").Value = 5
    Sheet1.Range("A2").Value = "t"
    v = Application.Evaluate("2^1024")
    R = DmpX(v) & "|" & Err.Number
    Sheet1.Cells.Clear
Done:
    M49 = R
    Exit Function
EH:
    M49 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M50() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(StrConv("zebra", vbProperCase)) & Hx(StrConv("zebra", vbUpperCase)) & Hx(StrConv("zebra", vbLowerCase)) & "|" & Hx(Left$("zebra", 2)) & Hx(Right$("zebra", 2)) & Hx(Mid$("zebra", 2, 2)) & "|" & Hx(LTrim(RTrim("zebra"))) & Hx(Trim$("zebra")) & "|" & Len("zebra") & LenB("zebra") & "|" & Hx(Space(2) & "zebra") & Hx(Replace("zebra", "a", "b", , , vbTextCompare))
    R = s
Done:
    M50 = R
    Exit Function
EH:
    M50 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M51() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As String, s As String, n As Long
    d = Sandbox()
    On Error Resume Next
    Open d & "\a.txt" For Output As #1: Print #1, "x": Close #1: SetAttr d & "\a.txt", vbHidden
    f = Dir(d & "\*.TXT", 99)
    Do While Len(f) > 0 And n < 12
        s = s & f & ";"
        n = n + 1
        f = Dir()
    Loop
    s = s & "|" & Err.Number
    Err.Clear
    f = Dir()
    s = s & "|" & Err.Number & "|" & Dir(d & "\nonexist.*") & "|" & Err.Number
    Wipe d
    R = s
Done:
    M51 = R
    Exit Function
EH:
    M51 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M52() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, a As Variant, b As Variant, c As Variant, e As Variant, l As String
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\w.txt" For Output As #f
    Write #f, "q", 5, 2.5, True, Null, "z"
    Write #f, "r", 6
    Write #f,
    Print #f, "tail,1,2"
    Close #f
    s = Err.Number & ""
    Open d & "\w.txt" For Input As #f
    Input #f, a, b, c, e
    s = s & "|" & Dmp(a) & Dmp(b) & Dmp(c) & Dmp(e)
    Input #f, a, b
    s = s & "|" & Dmp(a) & Dmp(b)
    Input #f, a
    s = s & "|" & Dmp(a) & Err.Number
    Line Input #f, l
    s = s & "|" & l
    Input #f, a, b
    s = s & "|" & Dmp(a) & Dmp(b) & Err.Number & EOF(f)
    Input #f, a
    s = s & "|" & Err.Number
    Close #f
    Wipe d
    R = s
Done:
    M52 = R
    Exit Function
EH:
    M52 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M53() As Variant
    On Error GoTo EH
    Dim R As Variant
    R10Log = ""
    On Error Resume Next
    ChainA 0, 2, 5
    R = R10Log & "|" & ErrS() & "|" & Erl
    R10Log = ""
Done:
    M53 = R
    Exit Function
EH:
    M53 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M54() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As String, s As String, n As Long
    d = Sandbox()
    On Error Resume Next
    Open d & "\a.txt" For Output As #1: Print #1, "x": Close #1: SetAttr d & "\a.txt", vbHidden
    f = Dir(d & "\*", vbDirectory)
    Do While Len(f) > 0 And n < 12
        s = s & f & ";"
        n = n + 1
        f = Dir()
    Loop
    s = s & "|" & Err.Number
    Err.Clear
    f = Dir()
    s = s & "|" & Err.Number & "|" & Dir(d & "\nonexist.*") & "|" & Err.Number
    Wipe d
    R = s
Done:
    M54 = R
    Exit Function
EH:
    M54 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M55() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Currency, c2 As Currency, s As String, d As Double
    c = 0.1: c2 = 7.7777
    s = c & "|" & c2 & "|" & (c + c2) & "|" & (c * c2) & "|" & TypeName(c * c2) & "|"
    On Error Resume Next
    s = s & (c / c2) & "|" & Err.Number & "|" & (c \ c2) & "|" & (c Mod c2) & "|" & Err.Number
    d = c
    s = s & "|" & d & "|" & CCur(d) & "|" & CStr(c) & "|" & Format(c, "0.0000") & "|" & CLng(c) & "|" & Int(c) & "|" & Fix(c) & "|" & Round(c, 2) & "|" & Hx(Format$(c))
    R = s
Done:
    M55 = R
    Exit Function
EH:
    M55 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M56() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Hx(Format(1234.5, "general number")) & "|" & Hx(Format$(1234.5, "general number", vbMonday, vbFirstFourDays)) & "|" & Err.Number
    R = s
Done:
    M56 = R
    Exit Function
EH:
    M56 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M57() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Columns("B:C").Select
    s = R10Log & "|" & Err.Number & "|" & ActiveSheet.Name & "|" & ActiveCell.Address(False, False) & "|" & Selection.Address(False, False)
    Err.Clear
    Sheet1.Activate
    Sheet1.Range("A1").Select
    R10Reset
    R10Log = ""
    R = s
Done:
    M57 = R
    Exit Function
EH:
    M57 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M58() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Integer, l As Long, b As Byte, c As Currency, d As Variant
    On Error Resume Next
    i = -1 + 32767
    s = i & "," & Err.Number & ";"
    Err.Clear
    l = -2147483647 - 1 + 65536
    s = s & l & "," & Err.Number & ";"
    Err.Clear
    b = 0 + 1
    s = s & b & "," & Err.Number & ";"
    Err.Clear
    c = 1234.5678 + 10
    s = s & c & "," & Err.Number & ";"
    Err.Clear
    d = -2147483647 - 1 + 65536
    s = s & Dmp(d) & "," & Err.Number
    R = s
Done:
    M58 = R
    Exit Function
EH:
    M58 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M59() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Err.Raise 13, Null, "d"
    s = ErrS() & "|" & Err.LastDllError
    R = s
Done:
    M59 = R
    Exit Function
EH:
    M59 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M60() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(CCur(2.5), CCur(3.5), CDec(2.5), CDec(3.5), CDec(-0.5))
        s = s & Try(7, v) & ","
    Next v
    s = s & "|" & Dmp(Round(0.5)) & Dmp(Round(0.5, -2)) & Dmp(Round(CDec(0.5), -2))
    R = s
Done:
    M60 = R
    Exit Function
EH:
    M60 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M61() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String, n As Long
    On Error GoTo H
    For i = 1 To 3
        s = s & i
        If i = 3 Then n = n / Z0()
        s = s & "."
    Next i
    R = s & "|" & n & "|" & Err.Number
    GoTo Done
H:
    s = s & "[" & Err.Number & "@" & i & "]"
    Resume Next
Skip1:
    R = R & "~" & s
Done:
    M61 = R
    Exit Function
EH:
    M61 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M62() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, a As Variant, b As Variant, c As Variant, e As Variant, l As String
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\w.txt" For Output As #f
    Write #f, "q", 5, 2.5, True, Null, "z"
    Write #f, "r", 6
    Write #f,
    Print #f, "tail,1,2"
    Close #f
    s = Err.Number & ""
    Open d & "\w.txt" For Input As #f
    Input #f, a, b, c, e
    s = s & "|" & Dmp(a) & Dmp(b) & Dmp(c) & Dmp(e)
    Input #f, a, b
    s = s & "|" & Dmp(a) & Dmp(b)
    Input #f, a
    s = s & "|" & Dmp(a) & Err.Number
    Line Input #f, l
    s = s & "|" & l
    Input #f, a, b
    s = s & "|" & Dmp(a) & Dmp(b) & Err.Number & EOF(f)
    Input #f, a
    s = s & "|" & Err.Number
    Close #f
    Wipe d
    R = s
Done:
    M62 = R
    Exit Function
EH:
    M62 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M63() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(#1/2/2000#, #12/31/1999 11:59:59 PM#, #3:04:05 AM#, #1/1/1900#, #2/29/2000 1:02:03 PM#, 0.5, 36526.75)
        s = s & Hx(Format$(v, "Long Date")) & ";"
    Next v
    R = s
Done:
    M63 = R
    Exit Function
EH:
    M63 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M64() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(StrConv("stra" & ChrW(223) & "e", vbProperCase)) & Hx(StrConv("stra" & ChrW(223) & "e", vbUpperCase)) & Hx(StrConv("stra" & ChrW(223) & "e", vbLowerCase)) & "|" & Hx(Left$("stra" & ChrW(223) & "e", 2)) & Hx(Right$("stra" & ChrW(223) & "e", 2)) & Hx(Mid$("stra" & ChrW(223) & "e", 2, 2)) & "|" & Hx(LTrim(RTrim("stra" & ChrW(223) & "e"))) & Hx(Trim$("stra" & ChrW(223) & "e")) & "|" & Len("stra" & ChrW(223) & "e") & LenB("stra" & ChrW(223) & "e") & "|" & Hx(Space(2) & "stra" & ChrW(223) & "e") & Hx(Replace("stra" & ChrW(223) & "e", "a", "b", , , vbTextCompare))
    R = s
Done:
    M64 = R
    Exit Function
EH:
    M64 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M65() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Range("A1").Value = 1
    s = R10Log
    R10Log = ""
    Application.EnableEvents = False
    Sheet1.Range("A1").Value = 2
    Sheet2.Range("A1").Value = 3
    s = s & "|" & R10Log
    R10Log = ""
    Application.EnableEvents = True
    Sheet2.Range("A1").Value = 4
    Sheet1.Range("A1:A2").Value = 5
    s = s & "|" & R10Log & "|" & Err.Number
    R10Reset
    R10Log = ""
    Sheet1.Cells.Clear: Sheet2.Cells.Clear
    R = s
Done:
    M65 = R
    Exit Function
EH:
    M65 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M66() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Currency, c2 As Currency, s As String, d As Double
    c = -1.23455: c2 = 0.0001
    s = c & "|" & c2 & "|" & (c + c2) & "|" & (c * c2) & "|" & TypeName(c * c2) & "|"
    On Error Resume Next
    s = s & (c / c2) & "|" & Err.Number & "|" & (c \ c2) & "|" & (c Mod c2) & "|" & Err.Number
    d = c
    s = s & "|" & d & "|" & CCur(d) & "|" & CStr(c) & "|" & Format(c, "0.0000") & "|" & CLng(c) & "|" & Int(c) & "|" & Fix(c) & "|" & Round(c, 2) & "|" & Hx(Format$(c))
    R = s
Done:
    M66 = R
    Exit Function
EH:
    M66 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M67() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hex(3.5) & "|" & Oct(3.5) & "|" & Hex$(16) & "|" & Len(Hex(3.5))
    On Error Resume Next
    s = s & "|" & Hex(True) & "|" & Err.Number & "|" & Hex(Null) & Err.Number & "|" & Oct(-1) & "|" & Hex(-32768) & Err.Number
    R = s
Done:
    M67 = R
    Exit Function
EH:
    M67 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M68() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(2147483647.5, 2147483648, -2147483648.5, -2147483649, 2147483646.5)
        s = s & Try(13, v) & ","
    Next v
    R = s
Done:
    M68 = R
    Exit Function
EH:
    M68 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M69() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(0.5, 1.5, 2.5, 3.5, -0.5, -1.5, -2.5)
        s = s & Try(7, v) & ","
    Next v
    s = s & "|" & Dmp(Round(-1.005)) & Dmp(Round(-1.005, -2)) & Dmp(Round(CDec(-1.005), -2))
    R = s
Done:
    M69 = R
    Exit Function
EH:
    M69 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M70() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error GoTo H
    s = "a"
    ChainB 0, 0
    s = s & "z"
    R = s & R10Log
    R10Log = ""
    GoTo Done
H:
    s = s & "[H" & Err.Number & ":" & Err.Source & "]"
    Resume Next
Done:
    M70 = R
    Exit Function
EH:
    M70 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M71() As Variant
    On Error GoTo EH
    Dim R As Variant
    R10Log = ""
    ChainA 3, 1, 2
    R = R10Log
    R10Log = ""
Done:
    M71 = R
    Exit Function
EH:
    M71 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M72() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim src As New R10Src, k1 As New R10Sink
    R10Log = ""
    k1.Id = "q": k1.Mode = 1
    k1.Attach src
    R = src.Fire1(1)
    src.Fire2
    R = R & "|" & R10Log & "|" & TypeName(src)
    R10Log = ""
Done:
    M72 = R
    Exit Function
EH:
    M72 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M73() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String, n As Long
    On Error GoTo H
    For i = 1 To 3
        s = s & i
        If i = 2 Then n = n / Z0()
        s = s & "."
    Next i
    R = s & "|" & n & "|" & Err.Number
    GoTo Done
H:
    s = s & "[" & Err.Number & "@" & i & "]"
    Err.Clear: Resume Next
Skip1:
    R = R & "~" & s
Done:
    M73 = R
    Exit Function
EH:
    M73 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M74() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Hx(Format(Date, "CURRENCY")) & "|" & Hx(Format$(Date, "CURRENCY", vbSunday, 0)) & "|" & Err.Number
    R = s
Done:
    M74 = R
    Exit Function
EH:
    M74 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M75() As Variant
    On Error GoTo EH
    Dim R As Variant
    R10Log = ""
    ChainA 0, 1, 4
    R = R10Log
    R10Log = ""
Done:
    M75 = R
    Exit Function
EH:
    M75 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M76() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    On Error Resume Next
    v = CallByName(Sheet1.Range("A1"), "Nope", VbLet, 1, 1)
    R = DmpX(v) & "|" & Err.Number & "|" & Dmp(Sheet1.Range("A1").Value) & Dmp(Sheet1.Range("B2").Value)
    Sheet1.Cells.Clear
Done:
    M76 = R
    Exit Function
EH:
    M76 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M77() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(StrConv("ab", vbProperCase)) & Hx(StrConv("ab", vbUpperCase)) & Hx(StrConv("ab", vbLowerCase)) & "|" & Hx(Left$("ab", 2)) & Hx(Right$("ab", 2)) & Hx(Mid$("ab", 2, 2)) & "|" & Hx(LTrim(RTrim("ab"))) & Hx(Trim$("ab")) & "|" & Len("ab") & LenB("ab") & "|" & Hx(Space(2) & "ab") & Hx(Replace("ab", "a", "b", , , vbTextCompare))
    R = s
Done:
    M77 = R
    Exit Function
EH:
    M77 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M78() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant, n As Long
    n = 3
    On Error Resume Next
    v = Application.Run("RunByRef", n)
    R = Dmp(v) & n & "|" & Err.Number
    v = RunByRef(n)
    R = R & Dmp(v) & n
Done:
    M78 = R
    Exit Function
EH:
    M78 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M79() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Err.Raise 424, "mysrc", "mydesc"
    s = ErrS()
    Err.Clear
    s = s & "|" & ErrS()
    Err.Raise 424
    s = s & "|" & ErrS()
    Error 424
    s = s & "|" & ErrS()
    s = s & "|" & Error$(424) & "|" & Error(424) & "|" & Err.HelpContext & "|" & Err.HelpFile
    R = s
Done:
    M79 = R
    Exit Function
EH:
    M79 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M80() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hex(CByte(5)) & "|" & Oct(CByte(5)) & "|" & Hex$(&H7FFFFFFF) & "|" & Len(Hex(CByte(5)))
    On Error Resume Next
    s = s & "|" & Hex(True) & "|" & Err.Number & "|" & Hex(Null) & Err.Number & "|" & Oct(-1) & "|" & Hex(-32768) & Err.Number
    R = s
Done:
    M80 = R
    Exit Function
EH:
    M80 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M81() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    On Error Resume Next
    For Each v In Array(#2/5/2000 6:00:00 PM#, #12/31/1899#, #1/1/1900#)
        s = s & Dmp(Int(v)) & Dmp(Fix(v)) & Err.Number & ";"
        Err.Clear
    Next v
    s = s & "|" & Int(-0#) & Fix(-0.5) & "|" & Hex(Int(-1)) & "|" & CInt(-0.5) & CInt(0.5) & CInt(1.5) & CInt(2.5) & "|" & CLng(2.5) & CLng(3.5) & CLng(-2.5)
    R = s
Done:
    M81 = R
    Exit Function
EH:
    M81 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M82() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    On Error Resume Next
    v = CallByName(Sheet1.Range("A1"), "Offset", VbMethod, "=1+1")
    R = DmpX(v) & "|" & Err.Number & "|" & Dmp(Sheet1.Range("A1").Value) & Dmp(Sheet1.Range("B2").Value)
    Sheet1.Cells.Clear
Done:
    M82 = R
    Exit Function
EH:
    M82 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M83() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As String, s As String, n As Long
    d = Sandbox()
    On Error Resume Next
    Open d & "\a.txt" For Output As #1: Close #1: MkDir d & "\sub"
    f = Dir(d & "\*.txt", vbDirectory + vbHidden)
    Do While Len(f) > 0 And n < 12
        s = s & f & ";"
        n = n + 1
        f = Dir()
    Loop
    s = s & "|" & Err.Number
    Err.Clear
    f = Dir()
    s = s & "|" & Err.Number & "|" & Dir(d & "\nonexist.*") & "|" & Err.Number
    Wipe d
    R = s
Done:
    M83 = R
    Exit Function
EH:
    M83 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M84() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Range("A3:A1").Value = 3
    s = R10Log & "|" & Err.Number
    Err.Clear
    R10Reset
    R10Log = ""
    R = s & "|" & Dmp(Sheet1.Range("A1").Value) & Dmp(Sheet1.Range("A2").Value) & Dmp(Sheet1.Range("A3").Value)
    Sheet1.Cells.Clear
Done:
    M84 = R
    Exit Function
EH:
    M84 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M85() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, p As Variant, t As Variant
    For Each t In Array("caf" & ChrW(233), "CAF" & ChrW(201), "cafe", "caf?")
        For Each p In Array("caf?", "CAF*", "[a-z]af" & ChrW(233), "[" & ChrW(224) & "-" & ChrW(255) & "]", "*" & ChrW(201))
            On Error Resume Next
            Err.Clear
            s = s & -(t Like p) & Err.Number
        Next p
        s = s & ";"
    Next t
    R = s
Done:
    M85 = R
    Exit Function
EH:
    M85 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M86() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As String, b As String
    a = "a-b": b = "ABC"
    On Error Resume Next
    s = StrComp(a, b) & StrComp(a, b, vbBinaryCompare) & StrComp(a, b, vbTextCompare) & StrComp(a, b, vbDatabaseCompare) & "|" & Err.Number
    Err.Clear
    s = s & "|" & (a = b) & (a < b) & (a > b) & (a <> b) & "|" & InStr(1, a & b, Left(b, 1), vbTextCompare) & InStr(1, a & b, Left(b, 1), vbBinaryCompare) & InStr(a, b) & "|" & InStrRev(a & b, Left(b, 1), -1, vbTextCompare) & "|" & Replace(a & b, Left(b, 1), "_", 1, -1, vbTextCompare) & Replace(a & b, Left(b, 1), "_") & "|" & (a Like b) & "|" & Hx(UCase(a)) & Hx(LCase(b)) & "|" & StrComp(a, b, 7) & Err.Number
    R = s
Done:
    M86 = R
    Exit Function
EH:
    M86 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M87() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As String, s As String, n As Long
    d = Sandbox()
    On Error Resume Next
    Open d & "\a.txt" For Output As #1: Print #1, "x": Close #1: SetAttr d & "\a.txt", vbHidden
    f = Dir(d & "\*.TXT", vbNormal)
    Do While Len(f) > 0 And n < 12
        s = s & f & ";"
        n = n + 1
        f = Dir()
    Loop
    s = s & "|" & Err.Number
    Err.Clear
    f = Dir()
    s = s & "|" & Err.Number & "|" & Dir(d & "\nonexist.*") & "|" & Err.Number
    Wipe d
    R = s
Done:
    M87 = R
    Exit Function
EH:
    M87 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M88() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Err.Raise 6, "mysrc", "mydesc"
    s = ErrS()
    Err.Clear
    s = s & "|" & ErrS()
    Err.Raise 6
    s = s & "|" & ErrS()
    Error 6
    s = s & "|" & ErrS()
    s = s & "|" & Error$(6) & "|" & Error(6) & "|" & Err.HelpContext & "|" & Err.HelpFile
    R = s
Done:
    M88 = R
    Exit Function
EH:
    M88 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M89() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Currency, c2 As Currency, s As String, d As Double
    c = -1.23455: c2 = 0.0001
    s = c & "|" & c2 & "|" & (c + c2) & "|" & (c * c2) & "|" & TypeName(c * c2) & "|"
    On Error Resume Next
    s = s & (c / c2) & "|" & Err.Number & "|" & (c \ c2) & "|" & (c Mod c2) & "|" & Err.Number
    d = c
    s = s & "|" & d & "|" & CCur(d) & "|" & CStr(c) & "|" & Format(c, "0.0000") & "|" & CLng(c) & "|" & Int(c) & "|" & Fix(c) & "|" & Round(c, 2) & "|" & Hx(Format$(c))
    R = s
Done:
    M89 = R
    Exit Function
EH:
    M89 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M90() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = Hx(Format(0.256, "Long Date")) & "|" & Hx(Format$(0.256, "Long Date", vbMonday, 4)) & "|" & Err.Number
    R = s
Done:
    M90 = R
    Exit Function
EH:
    M90 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M91() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, x As Single
    x = Rnd(-1)
    Randomize 1
    For i = 1 To 4
        s = s & Format(Rnd, "0.000000000") & ","
    Next i
    s = s & Format(Rnd(0), "0.0000000") & "," & Format(Rnd(1), "0.0000000") & "," & Format(Rnd(-7), "0.0000000") & "," & Format(Rnd, "0.0000000") & "," & Format(Rnd(2), "0.0000000")
    Randomize 1
    s = s & "|" & Format(Rnd, "0.000000000") & "|" & TypeName(Rnd) & TypeName(Rnd(7))
    R = s
Done:
    M91 = R
    Exit Function
EH:
    M91 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M92() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Range("A1").Value = 5: Sheet1.Range("A2").Formula = "=A1*2": Sheet1.Range("A1").Value = 6
    s = R10Log & "|" & Err.Number
    Err.Clear
    R10Reset
    R10Log = ""
    R = s & "|" & Dmp(Sheet1.Range("A1").Value) & Dmp(Sheet1.Range("A2").Value) & Dmp(Sheet1.Range("A3").Value)
    Sheet1.Cells.Clear
Done:
    M92 = R
    Exit Function
EH:
    M92 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M93() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, a As Variant, b As Variant, c As Variant, e As Variant, l As String
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\w.txt" For Output As #f
    Write #f, "q", 5, 2.5, True, Null, "z"
    Write #f, "r", 6
    Write #f,
    Print #f, "tail,1,2"
    Close #f
    s = Err.Number & ""
    Open d & "\w.txt" For Input As #f
    Input #f, a, b, c, e
    s = s & "|" & Dmp(a) & Dmp(b) & Dmp(c) & Dmp(e)
    Input #f, a, b
    s = s & "|" & Dmp(a) & Dmp(b)
    Input #f, a
    s = s & "|" & Dmp(a) & Err.Number
    Line Input #f, l
    s = s & "|" & l
    Input #f, a, b
    s = s & "|" & Dmp(a) & Dmp(b) & Err.Number & EOF(f)
    Input #f, a
    s = s & "|" & Err.Number
    Close #f
    Wipe d
    R = s
Done:
    M93 = R
    Exit Function
EH:
    M93 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M94() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String, n As Long
    On Error GoTo H
    For i = 1 To 3
        s = s & i
        If i = 3 Then n = n / Z0()
        s = s & "."
    Next i
    R = s & "|" & n & "|" & Err.Number
    GoTo Done
H:
    s = s & "[" & Err.Number & "@" & i & "]"
    i = i + 1: Resume Next
Skip1:
    R = R & "~" & s
Done:
    M94 = R
    Exit Function
EH:
    M94 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M95() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Range("A1").Value = 1
    s = R10Log
    R10Log = ""
    Application.EnableEvents = False
    Sheet1.Range("A1").Value = 2
    Sheet2.Range("A1").Value = 3
    s = s & "|" & R10Log
    R10Log = ""
    Application.EnableEvents = True
    Sheet2.Range("A1").Value = 4
    Sheet1.Range("A1:A2").Value = 5
    s = s & "|" & R10Log & "|" & Err.Number
    R10Reset
    R10Log = ""
    Sheet1.Cells.Clear: Sheet2.Cells.Clear
    R = s
Done:
    M95 = R
    Exit Function
EH:
    M95 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M96() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error GoTo H1
    s = "x"
    i = CLng("q")
    s = s & "1"
    On Error GoTo H2
    i = 1 / Z0()
    s = s & "2"
    On Error GoTo 0
    i = 1 / Z0()
    s = s & "3"
    R = s
    GoTo Done
H1:
    s = s & "(h1:" & Err.Number & ")"
    Resume Next
H2:
    s = s & "(h2:" & Err.Number & ")"
    Resume Next
Done:
    M96 = R
    Exit Function
EH:
    M96 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M97() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(0.5, 1.5, 2.5, 3.5, -0.5, -1.5, -2.5, 32767.5, 32768, -32768.5, -32769)
        s = s & Try(9, v) & ","
    Next v
    R = s
Done:
    M97 = R
    Exit Function
EH:
    M97 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M98() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Integer, l As Long, b As Byte, c As Currency, d As Variant
    On Error Resume Next
    i = 32767 Mod 0
    s = i & "," & Err.Number & ";"
    Err.Clear
    l = 2147483647 Mod 1
    s = s & l & "," & Err.Number & ";"
    Err.Clear
    b = 128 Mod 2
    s = s & b & "," & Err.Number & ";"
    Err.Clear
    c = 1234.5678 Mod 0.5
    s = s & c & "," & Err.Number & ";"
    Err.Clear
    d = 2147483647 Mod 1
    s = s & Dmp(d) & "," & Err.Number
    R = s
Done:
    M98 = R
    Exit Function
EH:
    M98 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M99() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, i As Long
    Dim t1 As String * 6, t2 As String * 6
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\rand.dat" For Random As #f Len = 6
    s = Err.Number & ":"
    For i = 1 To 3
        t1 = "rec" & i
        Put #f, 4 - i, t1
    Next i
    s = s & LOF(f) & "," & Loc(f) & "," & EOF(f) & "|" & Err.Number
    Get #f, 2, t2
    s = s & "|" & Hx(t2) & Err.Number
    Get #f, 9, t2
    s = s & "|" & Err.Number & Hx(t2) & EOF(f)
    Close #f
    s = s & "|" & FileLen(d & "\rand.dat")
    Wipe d
    R = s
Done:
    M99 = R
    Exit Function
EH:
    M99 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M100() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, i As Long
    Dim t1 As String * 6, t2 As String * 6
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\rand.dat" For Random As #f Len = 8
    s = Err.Number & ":"
    For i = 1 To 3
        t1 = "rec" & i
        Put #f, i, t1
    Next i
    s = s & LOF(f) & "," & Loc(f) & "," & EOF(f) & "|" & Err.Number
    Get #f, 2, t2
    s = s & "|" & Hx(t2) & Err.Number
    Get #f, 9, t2
    s = s & "|" & Err.Number & Hx(t2) & EOF(f)
    Close #f
    s = s & "|" & FileLen(d & "\rand.dat")
    Wipe d
    R = s
Done:
    M100 = R
    Exit Function
EH:
    M100 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M101() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, s As String, f As Integer, l As String
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\e.txt" For Binary As #f
    Put #f, , "a" & vbCr & "b" & vbLf & "c" & vbCrLf & "d" & vbCr & vbLf & "e" & Chr(26) & "f"
    Close #f
    Open d & "\e.txt" For Input As #f
    Do While Not EOF(f)
        Line Input #f, l
        s = s & "[" & Hx(l) & "]"
        If Len(s) > 200 Then Exit Do
    Loop
    Close #f
    Open d & "\e.txt" For Input As #f
    s = s & "|" & Hx(Input(8, #f)) & Err.Number
    Close #f
    Wipe d
    R = s
Done:
    M101 = R
    Exit Function
EH:
    M101 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M102() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    On Error Resume Next
    Sheet1.Range("B2").Value = 9
    v = Sheet1.Evaluate("ROW()")
    R = DmpX(v) & "|" & Err.Number
    v = Evaluate("ROW()")
    R = R & "|" & DmpX(v) & "|" & Err.Number
    Sheet1.Cells.Clear
Done:
    M102 = R
    Exit Function
EH:
    M102 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M103() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    On Error Resume Next
    v = Application.Run("RunEcho")
    R = Dmp(v) & "|" & Err.Number & "|" & Err.Description
Done:
    M103 = R
    Exit Function
EH:
    M103 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M104() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object, v As Variant
    R10Log = ""
    Set o = New R10Src
    On Error Resume Next
    v = CallByName(o, "FIRE1", VbGet, 3)
    R = Dmp(v) & "|" & Err.Number & ":" & Err.Description & "|" & R10Log
    R10Log = ""
Done:
    M104 = R
    Exit Function
EH:
    M104 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M105() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(Join(Split("abc", "A", 1, vbBinaryCompare), "/")) & "|" & UBound(Split("abc", "A", 1, vbBinaryCompare)) & "|" & UBound(Filter(Array("abc", "12", "x"), "a", True, vbBinaryCompare))
    R = s
Done:
    M105 = R
    Exit Function
EH:
    M105 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M106() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = Hx(StrConv("Apple", vbProperCase)) & Hx(StrConv("Apple", vbUpperCase)) & Hx(StrConv("Apple", vbLowerCase)) & "|" & Hx(Left$("Apple", 2)) & Hx(Right$("Apple", 2)) & Hx(Mid$("Apple", 2, 2)) & "|" & Hx(LTrim(RTrim("Apple"))) & Hx(Trim$("Apple")) & "|" & Len("Apple") & LenB("Apple") & "|" & Hx(Space(2) & "Apple") & Hx(Replace("Apple", "a", "b", , , vbTextCompare))
    R = s
Done:
    M106 = R
    Exit Function
EH:
    M106 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M107() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As String, s As String, n As Long
    d = Sandbox()
    On Error Resume Next
    Open d & "\a.txt" For Output As #1: Print #1, "x": Close #1: SetAttr d & "\a.txt", vbHidden
    f = Dir(d & "\b.d?t")
    Do While Len(f) > 0 And n < 12
        s = s & f & ";"
        n = n + 1
        f = Dir()
    Loop
    s = s & "|" & Err.Number
    Err.Clear
    f = Dir()
    s = s & "|" & Err.Number & "|" & Dir(d & "\nonexist.*") & "|" & Err.Number
    Wipe d
    R = s
Done:
    M107 = R
    Exit Function
EH:
    M107 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M108() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As String, s As String, n As Long
    d = Sandbox()
    On Error Resume Next
    Open d & "\a.txt" For Output As #1: Print #1, "x": Close #1: SetAttr d & "\a.txt", vbHidden
    f = Dir(d & "\?.txt")
    Do While Len(f) > 0 And n < 12
        s = s & f & ";"
        n = n + 1
        f = Dir()
    Loop
    s = s & "|" & Err.Number
    Err.Clear
    f = Dir()
    s = s & "|" & Err.Number & "|" & Dir(d & "\nonexist.*") & "|" & Err.Number
    Wipe d
    R = s
Done:
    M108 = R
    Exit Function
EH:
    M108 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M109() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object, v As Variant
    R10Log = ""
    Set o = New R10Src
    On Error Resume Next
    v = CallByName(o, "Fire1U", VbLet, 3)
    R = Dmp(v) & "|" & Err.Number & ":" & Err.Description & "|" & R10Log
    R10Log = ""
Done:
    M109 = R
    Exit Function
EH:
    M109 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M110() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Variant, b As Variant, v As Variant
    For Each a In Array(CInt(32767), CInt(-32768), CByte(255), CLng(2147483647))
        For Each b In Array(0.5, 1.5, 3, -3, 1E10)
            On Error Resume Next
            Err.Clear
            v = Empty
            v = a Mod b
            s = s & Dmp(v) & Err.Number & ";"
        Next b
    Next a
    R = s
Done:
    M110 = R
    Exit Function
EH:
    M110 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M111() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As String, f As Integer, s As String, l As String, y As Long
    d = Sandbox()
    On Error Resume Next
    f = FreeFile
    Open d & "\t.txt" For Output As #f
    Print #f, "line one"
    Print #f, "a", "b"; "c"
    Print #f, 12; -3.5; True
    Print #f, Tab(5); "tab"; Spc(3); "spc"
    Print #f, "a"; "b"
    Write #f, "q", 5, 2.5, True, Null, #1/2/2000#
    Write #f, "x,y", """quoted"""
    s = LOF(f) & "," & Loc(f) & "," & EOF(f)
    Close #f
    s = s & "|" & FileLen(d & "\t.txt") & "|" & Err.Number
    f = FreeFile
    Open d & "\t.txt" For Input As #f
    Do While Not EOF(f) And y < 12
        Line Input #f, l
        s = s & "[" & l & "]"
        y = y + 1
    Loop
    s = s & EOF(f) & "," & Loc(f) & "," & LOF(f)
    Close #f
    s = s & "|" & Err.Number
    Wipe d
    R = s
Done:
    M111 = R
    Exit Function
EH:
    M111 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M112() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Range("A1").Value = 5: Sheet1.Range("A1").NumberFormat = "0.00"
    s = R10Log & "|" & Err.Number
    Err.Clear
    R10Reset
    R10Log = ""
    R = s & "|" & Dmp(Sheet1.Range("A1").Value) & Dmp(Sheet1.Range("A2").Value) & Dmp(Sheet1.Range("A3").Value)
    Sheet1.Cells.Clear
Done:
    M112 = R
    Exit Function
EH:
    M112 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M113() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 6
    On Error Resume Next
    Sheet1.Range("A1").Value = 2
    Sheet1.Range("A2").Formula = "=A1*2"
    Sheet2.Range("A1").Formula = "=Sheet1!A2+1"
    R10Log = ""
    Application.EnableEvents = False: Sheet1.Range("A1").Value = 5: Application.EnableEvents = True
    s = R10Log & "|" & Err.Number & "|" & Sheet1.Range("A2").Value & "|" & Sheet2.Range("A1").Value & "|" & Application.Calculation
    Err.Clear
    R10Reset
    Application.Calculate
    s = s & "|" & Sheet2.Range("A1").Value
    R10Log = ""
    Sheet1.Cells.Clear: Sheet2.Cells.Clear
    R = s
Done:
    M113 = R
    Exit Function
EH:
    M113 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M114() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Err.Raise 5, "mysrc", "mydesc"
    s = ErrS()
    Err.Clear
    s = s & "|" & ErrS()
    Err.Raise 5
    s = s & "|" & ErrS()
    Error 5
    s = s & "|" & ErrS()
    s = s & "|" & Error$(5) & "|" & Error(5) & "|" & Err.HelpContext & "|" & Err.HelpFile
    R = s
Done:
    M114 = R
    Exit Function
EH:
    M114 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M115() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As String, b As String
    a = "strasse": b = "12"
    On Error Resume Next
    s = StrComp(a, b) & StrComp(a, b, vbBinaryCompare) & StrComp(a, b, vbTextCompare) & StrComp(a, b, vbDatabaseCompare) & "|" & Err.Number
    Err.Clear
    s = s & "|" & (a = b) & (a < b) & (a > b) & (a <> b) & "|" & InStr(1, a & b, Left(b, 1), vbTextCompare) & InStr(1, a & b, Left(b, 1), vbBinaryCompare) & InStr(a, b) & "|" & InStrRev(a & b, Left(b, 1), -1, vbTextCompare) & "|" & Replace(a & b, Left(b, 1), "_", 1, -1, vbTextCompare) & Replace(a & b, Left(b, 1), "_") & "|" & (a Like b) & "|" & Hx(UCase(a)) & Hx(LCase(b)) & "|" & StrComp(a, b, 7) & Err.Number
    R = s
Done:
    M115 = R
    Exit Function
EH:
    M115 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M116() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Range("A1").Value = 1: Sheet1.Range("A1").Value = 1
    s = R10Log & "|" & Err.Number
    Err.Clear
    R10Reset
    R10Log = ""
    R = s & "|" & Dmp(Sheet1.Range("A1").Value) & Dmp(Sheet1.Range("A2").Value) & Dmp(Sheet1.Range("A3").Value)
    Sheet1.Cells.Clear
Done:
    M116 = R
    Exit Function
EH:
    M116 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M117() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    R10Log = "": R10Mode = 0
    On Error Resume Next
    Sheet1.Range("A1").Formula = "=1+1"
    s = R10Log & "|" & Err.Number
    Err.Clear
    R10Reset
    R10Log = ""
    R = s & "|" & Dmp(Sheet1.Range("A1").Value) & Dmp(Sheet1.Range("A2").Value) & Dmp(Sheet1.Range("A3").Value)
    Sheet1.Cells.Clear
Done:
    M117 = R
    Exit Function
EH:
    M117 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M118() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Err.Raise -5, "mysrc", "mydesc"
    s = ErrS()
    Err.Clear
    s = s & "|" & ErrS()
    Err.Raise -5
    s = s & "|" & ErrS()
    Error -5
    s = s & "|" & ErrS()
    s = s & "|" & Error$(-5) & "|" & Error(-5) & "|" & Err.HelpContext & "|" & Err.HelpFile
    R = s
Done:
    M118 = R
    Exit Function
EH:
    M118 = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function M119() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error GoTo H1
    s = "x"
    i = CLng("q")
    s = s & "1"
    On Error GoTo H2
    i = 1
    s = s & "2"
    On Error GoTo 0
    i = 1 / Z0()
    s = s & "3"
    R = s
    GoTo Done
H1:
    s = s & "(h1:" & Err.Number & ")"
    Resume Next
H2:
    s = s & "(h2:" & Err.Number & ")"
    Resume Next
Done:
    M119 = R
    Exit Function
EH:
    M119 = "#ERR " & Err.Number & " " & Err.Description
End Function
