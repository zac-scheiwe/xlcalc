Attribute VB_Name = "ModVbFz1Ctl"
Option Explicit
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
Private Function D8(ByVal d As Double) As String
    Dim r As Double
    On Error Resume Next
    r = d - CDbl(CStr(d))
    If Err.Number <> 0 Then D8 = CStr(d) & "~?" Else D8 = CStr(d) & "~" & CStr(r)
End Function
Private Function Dmp(v As Variant) As String
    Dim s As String, i As Long, j As Long, l2 As Long, u2 As Long, nd As Long
    If IsObject(v) Then
        If v Is Nothing Then Dmp = "<Nothing>" Else Dmp = "<" & TypeName(v) & ">"
    ElseIf IsArray(v) Then
        On Error Resume Next
        s = TypeName(v) & LBound(v) & ":" & UBound(v)
        If Err.Number <> 0 Then Dmp = "A<" & Err.Number & ">": Exit Function
        nd = 1
        l2 = LBound(v, 2): u2 = UBound(v, 2)
        If Err.Number = 0 Then nd = 2: s = s & "," & l2 & ":" & u2
        Err.Clear
        s = s & "["
        If nd = 1 Then
            For i = LBound(v) To UBound(v)
                s = s & Dmp(v(i)) & ","
            Next i
        Else
            For i = LBound(v) To UBound(v)
                For j = l2 To u2
                    s = s & Dmp(v(i, j)) & ","
                Next j
                s = s & "/"
            Next i
        End If
        Dmp = s & "]"
    ElseIf IsNull(v) Then
        Dmp = "Null"
    ElseIf IsEmpty(v) Then
        Dmp = "Empty"
    ElseIf VarType(v) = vbDouble Or VarType(v) = vbSingle Then
        Dmp = TypeName(v) & ":" & D8(v)
    ElseIf VarType(v) = vbDate Then
        Dmp = "Date:" & D8(CDbl(v))
    ElseIf VarType(v) = vbError Then
        Dmp = "Err:" & CLng(v)
    Else
        Dmp = TypeName(v) & ":" & Hx(CStr(v))
    End If
End Function
Private Function Ap(v As Variant) As String
    If Err.Number <> 0 Then
        Ap = "#E" & Err.Number & ";"
        Err.Clear
    Else
        Ap = Dmp(v) & ";"
    End If
End Function
Private Function ErrS() As String
    ErrS = Err.Number & "/" & Err.Source & "/" & Hx(Err.Description) & ";"
    Err.Clear
End Function
Private Function Thrower(ByVal n As Long) As Variant
    Err.Raise n
    Thrower = "after"
End Function
Private Function Catcher(ByVal n As Long) As Variant
    On Error GoTo H
    Err.Raise n
    Catcher = "no"
    Exit Function
H:
    Catcher = "caught " & Err.Number
End Function
Private Function RetErr(ByVal n As Variant) As Variant
    RetErr = CVErr(n)
End Function
Public Function VbFz1Ctl(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1Ctl = C0()
    Case 1: VbFz1Ctl = C1()
    Case 2: VbFz1Ctl = C2()
    Case 3: VbFz1Ctl = C3()
    Case 4: VbFz1Ctl = C4()
    Case 5: VbFz1Ctl = C5()
    Case 6: VbFz1Ctl = C6()
    Case 7: VbFz1Ctl = C7()
    Case 8: VbFz1Ctl = C8()
    Case 9: VbFz1Ctl = C9()
    Case 10: VbFz1Ctl = C10()
    Case 11: VbFz1Ctl = C11()
    Case 12: VbFz1Ctl = C12()
    Case 13: VbFz1Ctl = C13()
    Case 14: VbFz1Ctl = C14()
    Case 15: VbFz1Ctl = C15()
    Case 16: VbFz1Ctl = C16()
    Case 17: VbFz1Ctl = C17()
    Case 18: VbFz1Ctl = C18()
    Case 19: VbFz1Ctl = C19()
    Case 20: VbFz1Ctl = C20()
    Case 21: VbFz1Ctl = C21()
    Case 22: VbFz1Ctl = C22()
    Case 23: VbFz1Ctl = C23()
    Case 24: VbFz1Ctl = C24()
    Case 25: VbFz1Ctl = C25()
    Case 26: VbFz1Ctl = C26()
    Case 27: VbFz1Ctl = C27()
    Case 28: VbFz1Ctl = C28()
    Case 29: VbFz1Ctl = C29()
    Case 30: VbFz1Ctl = C30()
    Case 31: VbFz1Ctl = C31()
    Case 32: VbFz1Ctl = C32()
    Case 33: VbFz1Ctl = C33()
    Case 34: VbFz1Ctl = C34()
    Case 35: VbFz1Ctl = C35()
    Case 36: VbFz1Ctl = C36()
    Case 37: VbFz1Ctl = C37()
    Case 38: VbFz1Ctl = C38()
    Case 39: VbFz1Ctl = C39()
    Case 40: VbFz1Ctl = C40()
    Case 41: VbFz1Ctl = C41()
    Case 42: VbFz1Ctl = C42()
    Case 43: VbFz1Ctl = C43()
    Case 44: VbFz1Ctl = C44()
    Case 45: VbFz1Ctl = C45()
    Case 46: VbFz1Ctl = C46()
    Case 47: VbFz1Ctl = C47()
    Case 48: VbFz1Ctl = C48()
    Case 49: VbFz1Ctl = C49()
    Case 50: VbFz1Ctl = C50()
    Case 51: VbFz1Ctl = C51()
    Case 52: VbFz1Ctl = C52()
    Case 53: VbFz1Ctl = C53()
    Case 54: VbFz1Ctl = C54()
    Case 55: VbFz1Ctl = C55()
    Case 56: VbFz1Ctl = C56()
    Case 57: VbFz1Ctl = C57()
    Case 58: VbFz1Ctl = C58()
    Case 59: VbFz1Ctl = C59()
    Case 60: VbFz1Ctl = C60()
    Case 61: VbFz1Ctl = C61()
    Case 62: VbFz1Ctl = C62()
    Case 63: VbFz1Ctl = C63()
    Case 64: VbFz1Ctl = C64()
    Case 65: VbFz1Ctl = C65()
    Case 66: VbFz1Ctl = C66()
    Case 67: VbFz1Ctl = C67()
    Case 68: VbFz1Ctl = C68()
    Case 69: VbFz1Ctl = C69()
    Case 70: VbFz1Ctl = C70()
    Case 71: VbFz1Ctl = C71()
    Case 72: VbFz1Ctl = C72()
    Case 73: VbFz1Ctl = C73()
    Case 74: VbFz1Ctl = C74()
    Case 75: VbFz1Ctl = C75()
    Case 76: VbFz1Ctl = C76()
    Case 77: VbFz1Ctl = C77()
    Case 78: VbFz1Ctl = C78()
    Case 79: VbFz1Ctl = C79()
    Case 80: VbFz1Ctl = C80()
    Case 81: VbFz1Ctl = C81()
    Case 82: VbFz1Ctl = C82()
    Case 83: VbFz1Ctl = C83()
    Case 84: VbFz1Ctl = C84()
    Case 85: VbFz1Ctl = C85()
    Case 86: VbFz1Ctl = C86()
    Case 87: VbFz1Ctl = C87()
    Case 88: VbFz1Ctl = C88()
    Case 89: VbFz1Ctl = C89()
    Case 90: VbFz1Ctl = C90()
    Case 91: VbFz1Ctl = C91()
    Case 92: VbFz1Ctl = C92()
    Case 93: VbFz1Ctl = C93()
    Case 94: VbFz1Ctl = C94()
    Case 95: VbFz1Ctl = C95()
    Case 96: VbFz1Ctl = C96()
    Case 97: VbFz1Ctl = C97()
    Case 98: VbFz1Ctl = C98()
    Case 99: VbFz1Ctl = C99()
    Case 100: VbFz1Ctl = C100()
    Case 101: VbFz1Ctl = C101()
    Case 102: VbFz1Ctl = C102()
    Case 103: VbFz1Ctl = C103()
    Case 104: VbFz1Ctl = C104()
    Case 105: VbFz1Ctl = C105()
    Case 106: VbFz1Ctl = C106()
    Case 107: VbFz1Ctl = C107()
    Case 108: VbFz1Ctl = C108()
    Case 109: VbFz1Ctl = C109()
    Case 110: VbFz1Ctl = C110()
    Case 111: VbFz1Ctl = C111()
    Case 112: VbFz1Ctl = C112()
    Case 113: VbFz1Ctl = C113()
    Case 114: VbFz1Ctl = C114()
    Case 115: VbFz1Ctl = C115()
    Case 116: VbFz1Ctl = C116()
    Case 117: VbFz1Ctl = C117()
    Case 118: VbFz1Ctl = C118()
    Case 119: VbFz1Ctl = C119()
    Case 120: VbFz1Ctl = C120()
    Case 121: VbFz1Ctl = C121()
    Case 122: VbFz1Ctl = C122()
    Case 123: VbFz1Ctl = C123()
    Case 124: VbFz1Ctl = C124()
    Case 125: VbFz1Ctl = C125()
    Case 126: VbFz1Ctl = C126()
    Case 127: VbFz1Ctl = C127()
    Case 128: VbFz1Ctl = C128()
    Case 129: VbFz1Ctl = C129()
    Case 130: VbFz1Ctl = C130()
    Case 131: VbFz1Ctl = C131()
    Case 132: VbFz1Ctl = C132()
    Case 133: VbFz1Ctl = C133()
    Case 134: VbFz1Ctl = C134()
    Case 135: VbFz1Ctl = C135()
    Case 136: VbFz1Ctl = C136()
    Case 137: VbFz1Ctl = C137()
    Case 138: VbFz1Ctl = C138()
    Case 139: VbFz1Ctl = C139()
    Case 140: VbFz1Ctl = C140()
    Case 141: VbFz1Ctl = C141()
    Case 142: VbFz1Ctl = C142()
    Case 143: VbFz1Ctl = C143()
    Case 144: VbFz1Ctl = C144()
    Case 145: VbFz1Ctl = C145()
    Case 146: VbFz1Ctl = C146()
    Case 147: VbFz1Ctl = C147()
    Case 148: VbFz1Ctl = C148()
    Case 149: VbFz1Ctl = C149()
    Case 150: VbFz1Ctl = C150()
    Case 151: VbFz1Ctl = C151()
    Case 152: VbFz1Ctl = C152()
    Case 153: VbFz1Ctl = C153()
    Case 154: VbFz1Ctl = C154()
    Case 155: VbFz1Ctl = C155()
    Case 156: VbFz1Ctl = C156()
    Case 157: VbFz1Ctl = C157()
    Case 158: VbFz1Ctl = C158()
    Case 159: VbFz1Ctl = C159()
    Case 160: VbFz1Ctl = C160()
    Case 161: VbFz1Ctl = C161()
    Case 162: VbFz1Ctl = C162()
    Case 163: VbFz1Ctl = C163()
    Case 164: VbFz1Ctl = C164()
    Case 165: VbFz1Ctl = C165()
    Case 166: VbFz1Ctl = C166()
    Case 167: VbFz1Ctl = C167()
    Case 168: VbFz1Ctl = C168()
    Case 169: VbFz1Ctl = C169()
    End Select
    Exit Function
EH:
    VbFz1Ctl = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Long
    For i = 1 To 3 Step 0
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    n = 0: For i = 253 To 2147483647 Step 2
        s = s & Ap(i): n = n + 1: If n > 6 Then Exit For
    Next i
    s = s & Ap(i) & ErrS()
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = -1
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C1 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = 2 ^ 1024
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(4): s = s & Ap(x) & ErrS()
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 47, "S", "D"
    s = s & ErrS()
    Err.Raise 450
    s = s & ErrS()
    Err.Raise 47
    Err.Raise 2
    s = s & ErrS()
    v = Error(40000): s = s & Ap(v)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 0
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C4 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 98, , "Desc"
    s = s & ErrS()
    Err.Raise 459
    s = s & ErrS()
    Err.Raise 98
    Err.Raise 381
    s = s & ErrS()
    v = Error(460): s = s & Ap(v)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 1
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 256
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C6 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = -1
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C7 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(1.5): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = CLng("x" & n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C9 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(2015): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = 0.007593
    Select Case x
    Case -2
        s = s & "c0"
    Case Is > -0.1
        s = s & "c1"
    Case "x-y" To (ChrW(198))
        s = s & "c2"
    Case "1.5" To "ABC"
        s = s & "c3"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    Do While n > 3
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop While n > -1 And n > -20
    s = s & n
    x = Choose(n + 1, "x", "y")
    s = s & Ap(x)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 2
    x = CLng("x" & n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C13 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(Null): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 440, "S", "D"
    s = s & ErrS()
    Err.Raise 999
    s = s & ErrS()
    Err.Raise 440
    Err.Raise 481
    s = s & ErrS()
    v = Error(442): s = s & Ap(v)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    Do While n <> 5
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < 2 Or n < -20
    s = s & n
    x = Choose(n + 1, "x", "y")
    s = s & Ap(x)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 1
    x = d \ d
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(448): s = s & Ap(x) & ErrS()
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(65535): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 1
    Do Until n > 4
        n = n + -1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop While n > 2 And n > -20
    s = s & n
    x = Choose(2.9, 1, 2, 3)
    s = s & Ap(x)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr("2042"): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(1.5): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    Do While n > 0
        n = n + -1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < -1 Or n < -20
    s = s & n
    x = Choose(-1, 1, 2, 3)
    s = s & Ap(x)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = CInt(32766)
    Select Case x
    Case Null
        s = s & "c0"
    Case "ABC" To ""
        s = s & "c1"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 1
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C24 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 4
    Do Until n < 1
        n = n + -1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop While n > 2 And n > -20
    s = s & n
    x = Choose(-1, 1, 2, 3)
    s = s & Ap(x)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(65536): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Single
    For i = 1 To 1.5
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For
        If n = 2 Then i = i + 1
    Next i
    s = s & Ap(i) & n
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = 10 / d
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(400): s = s & Ap(x) & ErrS()
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 2
    x = CInt(40000 * n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C29 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr("2042"): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 2
    x = Mid("abc", n - 1)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C31 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(2023): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = Array(1)(n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C33 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 256
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C34 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C35 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 0
    Do Until n < 6
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < -1 Or n < -20
    s = s & n
    x = Choose(n + 1, "x", "y")
    s = s & Ap(x)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 0
    Do While n > 6
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < 3 Or n < -20
    s = s & n
    x = IIf(n, 1 / n, "z")
    s = s & Ap(x)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = "A1"
    Select Case x
    Case -1 To 0
        s = s & "c0"
    Case True
        s = s & "c1"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 2
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C39 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = 1 / n
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C40 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    If n < 5 Then n = n + 1: Resume
    GoTo Back
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Currency
    For i = 0.3 To 5 Step 0.5
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 1
    x = 2 ^ 1024
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(432): s = s & Ap(x) & ErrS()
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(Null): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 438, , "Desc"
    s = s & ErrS()
    Err.Raise 98
    s = s & ErrS()
    Err.Raise 438
    Err.Raise 424
    s = s & ErrS()
    v = Error(735): s = s & Ap(v)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Currency
    For i = -2 To -1 Step 10
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = (ChrW(233))
    Select Case x
    Case -2 To 1
        s = s & "c0"
    Case Is <= Empty
        s = s & "c1"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 1
    x = 1 / n
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C47 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Integer
    For i = 1 To 1.5 Step 1.5
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    n = 0: For i = -32767 To 2147483647
        s = s & Ap(i): n = n + 1: If n > 6 Then Exit For
    Next i
    s = s & Ap(i) & ErrS()
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(65536): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 1
    x = Mid("abc", n - 1)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C50 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Back
    GoTo Back
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = d \ d
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(460): s = s & Ap(x) & ErrS()
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 1
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C52 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Variant
    For i = 1.5 To 1
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For
        If n = 2 Then i = i + 1
    Next i
    s = s & Ap(i) & n
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 1
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = -1
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C54 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Integer
    For i = -1 To "2" Step 2.5
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For
        If n = 2 Then i = i + 1
    Next i
    s = s & Ap(i) & n
    n = 0: For i = 32765 To 255 Step -1
        s = s & Ap(i): n = n + 1: If n > 6 Then Exit For
    Next i
    s = s & Ap(i) & ErrS()
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(2023): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Integer
    For i = 10 To 0.3 Step -0.5
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    n = 0: For i = 253 To -32768 Step -1
        s = s & Ap(i): n = n + 1: If n > 6 Then Exit For
    Next i
    s = s & Ap(i) & ErrS()
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 1
    x = 1 / n
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C58 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C59 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = 2 ^ 1024
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(458): s = s & Ap(x) & ErrS()
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = (ChrW(198))
    Select Case x
    Case 2 To 0
        s = s & "c0"
    Case 1
        s = s & "c1"
    Case -3 To 11
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 256
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C62 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = Thrower(432)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C63 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 2
    x = CLng("x" & n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C64 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 482
    s = s & ErrS()
    Err.Raise 999
    s = s & ErrS()
    Err.Raise 482
    Err.Raise 74
    s = s & ErrS()
    v = Error(100): s = s & Ap(v)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = True
    Select Case x
    Case Is > "m"
        s = s & "c0"
    Case Is > 1
        s = s & "c1"
    Case "1.5" To "2"
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 1
    x = Thrower(91)
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(51): s = s & Ap(x) & ErrS()
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = Thrower(53)
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(443): s = s & Ap(x) & ErrS()
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 482
    s = s & ErrS()
    Err.Raise 746
    s = s & ErrS()
    Err.Raise 482
    Err.Raise 400
    s = s & ErrS()
    v = Error(521): s = s & Ap(v)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = 10 / d
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(381): s = s & Ap(x) & ErrS()
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 53, , "Desc"
    s = s & ErrS()
    Err.Raise 443
    s = s & ErrS()
    Err.Raise 53
    Err.Raise 393
    s = s & ErrS()
    v = Error(54): s = s & Ap(v)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 0
    Do Until n > 4
        n = n + 2
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < 2 Or n < -20
    s = s & n
    x = Choose(1.5, 1, 2, 3)
    s = s & Ap(x)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    Do While n < 2
        n = n + 2
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < 0 Or n < -20
    s = s & n
    x = Choose(1.5, 1, 2, 3)
    s = s & Ap(x)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = "abc"
    Select Case x
    Case -2
        s = s & "c0"
    Case Is < True
        s = s & "c1"
    Case 10
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Integer
    For i = 32766 To 2.5 Step 0
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For
        If n = 2 Then i = i + 1
    Next i
    s = s & Ap(i) & n
    n = 0: For i = -32767 To 2147483647 Step -1
        s = s & Ap(i): n = n + 1: If n > 6 Then Exit For
    Next i
    s = s & Ap(i) & ErrS()
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = 1 / n
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C76 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    If n < 5 Then n = n + 1: Resume
    GoTo Back
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 382, , "Desc"
    s = s & ErrS()
    Err.Raise 381
    s = s & ErrS()
    Err.Raise 382
    Err.Raise 18
    s = s & ErrS()
    v = Error(1): s = s & Ap(v)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 4
    Do While n < 4
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < 3 Or n < -20
    s = s & n
    x = Switch(n > 2, "a", n < 0, "b")
    s = s & Ap(x)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 0
    x = 2 ^ 1024
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(96): s = s & Ap(x) & ErrS()
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 1
    x = d \ d
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(68): s = s & Ap(x) & ErrS()
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 0
    x = 10 / d
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(20): s = s & Ap(x) & ErrS()
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Variant
    For i = 255 To "2" Step 0
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 20, , "Desc"
    s = s & ErrS()
    Err.Raise 457
    s = s & ErrS()
    Err.Raise 20
    Err.Raise 485
    s = s & ErrS()
    v = Error(393): s = s & Ap(v)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = -5
    Select Case x
    Case 1, 1.5, Is > 7
        s = s & "c0"
    Case 32766, 255, Is > 8
        s = s & "c1"
    Case 3
        s = s & "c2"
    Case 1
        s = s & "c3"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(Null): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(2036): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = CLng("x" & n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C87 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = 4
    Select Case x
    Case 10, "2", Is > 5
        s = s & "c0"
    Case Is <> "m"
        s = s & "c1"
    Case Is > True
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = (-10)
    Select Case x
    Case -3 To 8
        s = s & "c0"
    Case Is > True
        s = s & "c1"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 0
    x = Array(1)(n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C90 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 0
    x = CLng("x" & n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C91 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 8, , "Desc"
    s = s & ErrS()
    Err.Raise 62
    s = s & ErrS()
    Err.Raise 8
    Err.Raise 451
    s = s & ErrS()
    v = Error(57): s = s & Ap(v)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 0
    Do Until n <> 1
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < 2 Or n < -20
    s = s & n
    x = Switch(n > 2, "a", n < 0, "b")
    s = s & Ap(x)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = "1.5"
    Select Case x
    Case Is > "5"
        s = s & "c0"
    Case 5
        s = s & "c1"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 0
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C95 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Single
    For i = -2 To 0.1 Step 0
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = (-1.6688)
    Select Case x
    Case 2.5, 10, Is > 0
        s = s & "c0"
    Case "2"
        s = s & "c1"
    Case ""
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = 2.5
    Select Case x
    Case -1 To 2
        s = s & "c0"
    Case 1.5, 255, Is > 4
        s = s & "c1"
    Case "z" To "1.5"
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = Array(1)(n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C99 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C100 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = #2/29/2000 12:00:00 PM#
    Select Case x
    Case Is <> Empty
        s = s & "c0"
    Case "a1" To "e"
        s = s & "c1"
    Case 1.5, 1.5, Is > 7
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = 2 ^ 1024
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(76): s = s & Ap(x) & ErrS()
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = CInt(40000 * n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C103 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    If n < 5 Then n = n + 1: Resume
    GoTo Back
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(2042): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 1
    x = Catcher(735)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C105 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = d \ d
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(429): s = s & Ap(x) & ErrS()
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 484
    s = s & ErrS()
    Err.Raise 3
    s = s & ErrS()
    Err.Raise 484
    Err.Raise 460
    s = s & ErrS()
    v = Error(2000): s = s & Ap(v)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C108 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(-1): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 480, , "Desc"
    s = s & ErrS()
    Err.Raise 52
    s = s & ErrS()
    Err.Raise 480
    Err.Raise 485
    s = s & ErrS()
    v = Error(97): s = s & Ap(v)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Variant
    For i = 5 To 10
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = CInt(40000 * n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C112 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    If n < 5 Then n = n + 1: Resume
    GoTo Back
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 1
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C113 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    Do Until n > 2
        n = n + -1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop While n > -1 And n > -20
    s = s & n
    x = Choose(0, 1, 2, 3)
    s = s & Ap(x)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C115 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 76
    s = s & ErrS()
    Err.Raise 448
    s = s & ErrS()
    Err.Raise 76
    Err.Raise 483
    s = s & ErrS()
    v = Error(448): s = s & Ap(v)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 0
    x = Array(1)(n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C117 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Back
    GoTo Back
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = "B"
    Select Case x
    Case "a"
        s = s & "c0"
    Case 2 To 11
        s = s & "c1"
    Case 3 To 7
        s = s & "c2"
    Case -4 To 2
        s = s & "c3"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = 2 ^ 1024
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(446): s = s & Ap(x) & ErrS()
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = "-1"
    Select Case x
    Case -2
        s = s & "c0"
    Case Is <> "5"
        s = s & "c1"
    Case Is <= True
        s = s & "c2"
    Case Is <> True
        s = s & "c3"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 1
    x = Thrower(455)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C121 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    If n < 5 Then n = n + 1: Resume
    GoTo Back
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 0
    Do Until n < 1
        n = n + 2
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop While n > 1 And n > -20
    s = s & n
    x = Switch(n > 2, "a", n < 0, "b")
    s = s & Ap(x)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 1
    x = CLng("x" & n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C123 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 422, , "Desc"
    s = s & ErrS()
    Err.Raise 422
    s = s & ErrS()
    Err.Raise 422
    Err.Raise 94
    s = s & ErrS()
    v = Error(455): s = s & Ap(v)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 454, , "Desc"
    s = s & ErrS()
    Err.Raise 16
    s = s & ErrS()
    Err.Raise 454
    Err.Raise 321
    s = s & ErrS()
    v = Error(482): s = s & Ap(v)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 256
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C126 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 40000, "Src"
    s = s & ErrS()
    Err.Raise 458
    s = s & ErrS()
    Err.Raise 40000
    Err.Raise 432
    s = s & ErrS()
    v = Error(91): s = s & Ap(v)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Double
    For i = 2.5 To 32766 Step 0.3
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For
        If n = 2 Then i = i + 1
    Next i
    s = s & Ap(i) & n
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 462, "S", "D"
    s = s & ErrS()
    Err.Raise 485
    s = s & ErrS()
    Err.Raise 462
    Err.Raise 68
    s = s & ErrS()
    v = Error(91): s = s & Ap(v)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 447, "Src"
    s = s & ErrS()
    Err.Raise 98
    s = s & ErrS()
    Err.Raise 447
    Err.Raise 419
    s = s & ErrS()
    v = Error(98): s = s & Ap(v)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = Array(1)(n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C131 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Back
    GoTo Back
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 322
    s = s & ErrS()
    Err.Raise 383
    s = s & ErrS()
    Err.Raise 322
    Err.Raise 451
    s = s & ErrS()
    v = Error(5): s = s & Ap(v)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Variant
    For i = 2.5 To 0.3 Step -0.5
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 0
    x = Thrower(385)
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(447): s = s & Ap(x) & ErrS()
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 2
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C135 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    Do While n > 6
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < -1 Or n < -20
    s = s & n
    x = Switch(n > 2, "a", n < 0, "b")
    s = s & Ap(x)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 1
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 4
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C137 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = CLng("x" & n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C138 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Back
    GoTo Back
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Integer
    For i = 0.5 To 255 Step "2"
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    n = 0: For i = 253 To 255 Step -1
        s = s & Ap(i): n = n + 1: If n > 6 Then Exit For
    Next i
    s = s & Ap(i) & ErrS()
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(-1): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    Do Until n <> 4
        n = n + -1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < 2 Or n < -20
    s = s & n
    x = Choose(0, 1, 2, 3)
    s = s & Ap(x)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 0
    x = Array(1)(n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C142 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 746, "Src"
    s = s & ErrS()
    Err.Raise 53
    s = s & ErrS()
    Err.Raise 746
    Err.Raise 438
    s = s & ErrS()
    v = Error(422): s = s & Ap(v)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 3
    GoSub L1
    On n GoTo T1, T2, T3
    s = s & "none"
    GoTo Fin
T1:
    s = s & "t1": GoTo Fin
T2:
    s = s & "t2": GoTo Fin
T3:
    s = s & "t3"
Fin:
    n = 2
    On n GoTo T4
    s = s & "skip" & ErrS()
T4:
    s = s & "end"
    C144 = s
    Exit Function
L1:
    s = s & "sub" & n
    Return
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(2036): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = Thrower(54)
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(92): s = s & Ap(x) & ErrS()
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = (-1.1)
    Select Case x
    Case -1, 0.1, Is > 0
        s = s & "c0"
    Case "[" To "z"
        s = s & "c1"
    Case True
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 1
    x = 10 / d
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(321): s = s & Ap(x) & ErrS()
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(2007): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C149 = s
End Function
Private Function C150() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Currency
    For i = 5 To 1 Step 10
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    C150 = s
End Function
Private Function C151() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Single
    For i = "2" To -0.1 Step 0
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    C151 = s
End Function
Private Function C152() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 0
    x = CInt(40000 * n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C152 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C152 = s
End Function
Private Function C153() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 0
    Do Until n > 5
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop Until n < 2 Or n < -20
    s = s & n
    x = Switch(n > 2, "a", n < 0, "b")
    s = s & Ap(x)
    C153 = s
End Function
Private Function C154() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 3
    x = CInt(40000 * n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C154 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Next
    GoTo Back
    C154 = s
End Function
Private Function C155() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = RetErr(2015): s = s & Ap(x)
    s = s & IsError(x) & VarType(x)
    v = Empty: v = x + 1: s = s & Ap(v)
    v = Empty: v = (x = CVErr(2042)): s = s & Ap(v)
    v = Empty: v = CStr(x): s = s & Ap(v)
    v = Empty: v = CLng(x): s = s & Ap(v)
    C155 = s
End Function
Private Function C156() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 1
    x = 2 ^ 1024
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(58): s = s & Ap(x) & ErrS()
    C156 = s
End Function
Private Function C157() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 457, "S", "D"
    s = s & ErrS()
    Err.Raise 2
    s = s & ErrS()
    Err.Raise 457
    Err.Raise 459
    s = s & ErrS()
    v = Error(62): s = s & Ap(v)
    C157 = s
End Function
Private Function C158() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 0
    x = 2 ^ 1024
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(1): s = s & Ap(x) & ErrS()
    C158 = s
End Function
Private Function C159() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = 3
    Select Case x
    Case Null
        s = s & "c0"
    Case 10
        s = s & "c1"
    Case 5 To 0
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C159 = s
End Function
Private Function C160() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    Do Until n <> 5
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop While n > -2 And n > -20
    s = s & n
    x = Switch(n > 2, "a", n < 0, "b")
    s = s & Ap(x)
    C160 = s
End Function
Private Function C161() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = (-4846)
    Select Case x
    Case 4 To 11
        s = s & "c0"
    Case 0.5, -0.1, Is > 2
        s = s & "c1"
    Case True
        s = s & "c2"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C161 = s
End Function
Private Function C162() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 13, , "Desc"
    s = s & ErrS()
    Err.Raise 446
    s = s & ErrS()
    Err.Raise 13
    Err.Raise 54
    s = s & ErrS()
    v = Error(402): s = s & Ap(v)
    C162 = s
End Function
Private Function C163() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Err.Raise 485, "S", "D"
    s = s & ErrS()
    Err.Raise 419
    s = s & ErrS()
    Err.Raise 485
    Err.Raise 381
    s = s & ErrS()
    v = Error(463): s = s & Ap(v)
    C163 = s
End Function
Private Function C164() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim i As Integer
    For i = 2.5 To 2.5 Step 0.1
        s = s & Ap(i): n = n + 1: If n > 30 Then Exit For

    Next i
    s = s & Ap(i) & n
    n = 0: For i = 2147483645 To 255 Step 2
        s = s & Ap(i): n = n + 1: If n > 6 Then Exit For
    Next i
    s = s & Ap(i) & ErrS()
    C164 = s
End Function
Private Function C165() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    x = 419419.502
    Select Case x
    Case -1 To 0
        s = s & "c0"
    Case 3 To 5
        s = s & "c1"
    Case Else
        s = s & "else"
    End Select
    s = s & ErrS()
    C165 = s
End Function
Private Function C166() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    n = 2
    Do Until n <> 6
        n = n + 1
        s = s & n & ","
        If Abs(n) > 12 Then Exit Do
    Loop
    Do
        n = n - 1
    Loop While n > 3 And n > -20
    s = s & n
    x = Choose(-1, 1, 2, 3)
    s = s & Ap(x)
    C166 = s
End Function
Private Function C167() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = Thrower(457)
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(7): s = s & Ap(x) & ErrS()
    C167 = s
End Function
Private Function C168() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    Dim d As Long
    d = 2
    x = Thrower(1000)
    s = s & ErrS() & Ap(x)
    On Error GoTo 0
    On Error Resume Next
    s = s & ErrS()
    x = Catcher(53): s = s & Ap(x) & ErrS()
    C168 = s
End Function
Private Function C169() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim n As Long, x As Variant
    On Error GoTo H
    n = 0
    x = CInt(40000 * n)
    s = s & "ok" & Ap(x)
Back:
    s = s & "back" & ErrS()
    C169 = s
    Exit Function
H:
    s = s & "H" & ErrS()
    Resume Back
    GoTo Back
    C169 = s
End Function
