Attribute VB_Name = "ModVbFz1VArith"
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
Public Function VbFz1VArith(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1VArith = C0()
    Case 1: VbFz1VArith = C1()
    Case 2: VbFz1VArith = C2()
    Case 3: VbFz1VArith = C3()
    Case 4: VbFz1VArith = C4()
    Case 5: VbFz1VArith = C5()
    Case 6: VbFz1VArith = C6()
    Case 7: VbFz1VArith = C7()
    Case 8: VbFz1VArith = C8()
    Case 9: VbFz1VArith = C9()
    Case 10: VbFz1VArith = C10()
    Case 11: VbFz1VArith = C11()
    Case 12: VbFz1VArith = C12()
    Case 13: VbFz1VArith = C13()
    Case 14: VbFz1VArith = C14()
    Case 15: VbFz1VArith = C15()
    Case 16: VbFz1VArith = C16()
    Case 17: VbFz1VArith = C17()
    Case 18: VbFz1VArith = C18()
    Case 19: VbFz1VArith = C19()
    Case 20: VbFz1VArith = C20()
    Case 21: VbFz1VArith = C21()
    Case 22: VbFz1VArith = C22()
    Case 23: VbFz1VArith = C23()
    Case 24: VbFz1VArith = C24()
    Case 25: VbFz1VArith = C25()
    Case 26: VbFz1VArith = C26()
    Case 27: VbFz1VArith = C27()
    Case 28: VbFz1VArith = C28()
    Case 29: VbFz1VArith = C29()
    Case 30: VbFz1VArith = C30()
    Case 31: VbFz1VArith = C31()
    Case 32: VbFz1VArith = C32()
    Case 33: VbFz1VArith = C33()
    Case 34: VbFz1VArith = C34()
    Case 35: VbFz1VArith = C35()
    Case 36: VbFz1VArith = C36()
    Case 37: VbFz1VArith = C37()
    Case 38: VbFz1VArith = C38()
    Case 39: VbFz1VArith = C39()
    Case 40: VbFz1VArith = C40()
    Case 41: VbFz1VArith = C41()
    Case 42: VbFz1VArith = C42()
    Case 43: VbFz1VArith = C43()
    Case 44: VbFz1VArith = C44()
    Case 45: VbFz1VArith = C45()
    Case 46: VbFz1VArith = C46()
    Case 47: VbFz1VArith = C47()
    Case 48: VbFz1VArith = C48()
    Case 49: VbFz1VArith = C49()
    Case 50: VbFz1VArith = C50()
    Case 51: VbFz1VArith = C51()
    Case 52: VbFz1VArith = C52()
    Case 53: VbFz1VArith = C53()
    Case 54: VbFz1VArith = C54()
    Case 55: VbFz1VArith = C55()
    Case 56: VbFz1VArith = C56()
    Case 57: VbFz1VArith = C57()
    Case 58: VbFz1VArith = C58()
    Case 59: VbFz1VArith = C59()
    Case 60: VbFz1VArith = C60()
    Case 61: VbFz1VArith = C61()
    Case 62: VbFz1VArith = C62()
    Case 63: VbFz1VArith = C63()
    Case 64: VbFz1VArith = C64()
    Case 65: VbFz1VArith = C65()
    Case 66: VbFz1VArith = C66()
    Case 67: VbFz1VArith = C67()
    Case 68: VbFz1VArith = C68()
    Case 69: VbFz1VArith = C69()
    Case 70: VbFz1VArith = C70()
    Case 71: VbFz1VArith = C71()
    Case 72: VbFz1VArith = C72()
    Case 73: VbFz1VArith = C73()
    Case 74: VbFz1VArith = C74()
    Case 75: VbFz1VArith = C75()
    Case 76: VbFz1VArith = C76()
    Case 77: VbFz1VArith = C77()
    Case 78: VbFz1VArith = C78()
    Case 79: VbFz1VArith = C79()
    Case 80: VbFz1VArith = C80()
    Case 81: VbFz1VArith = C81()
    Case 82: VbFz1VArith = C82()
    Case 83: VbFz1VArith = C83()
    Case 84: VbFz1VArith = C84()
    Case 85: VbFz1VArith = C85()
    Case 86: VbFz1VArith = C86()
    Case 87: VbFz1VArith = C87()
    Case 88: VbFz1VArith = C88()
    Case 89: VbFz1VArith = C89()
    Case 90: VbFz1VArith = C90()
    Case 91: VbFz1VArith = C91()
    Case 92: VbFz1VArith = C92()
    Case 93: VbFz1VArith = C93()
    Case 94: VbFz1VArith = C94()
    Case 95: VbFz1VArith = C95()
    Case 96: VbFz1VArith = C96()
    Case 97: VbFz1VArith = C97()
    Case 98: VbFz1VArith = C98()
    Case 99: VbFz1VArith = C99()
    Case 100: VbFz1VArith = C100()
    Case 101: VbFz1VArith = C101()
    Case 102: VbFz1VArith = C102()
    Case 103: VbFz1VArith = C103()
    Case 104: VbFz1VArith = C104()
    Case 105: VbFz1VArith = C105()
    Case 106: VbFz1VArith = C106()
    Case 107: VbFz1VArith = C107()
    Case 108: VbFz1VArith = C108()
    Case 109: VbFz1VArith = C109()
    Case 110: VbFz1VArith = C110()
    Case 111: VbFz1VArith = C111()
    Case 112: VbFz1VArith = C112()
    Case 113: VbFz1VArith = C113()
    Case 114: VbFz1VArith = C114()
    Case 115: VbFz1VArith = C115()
    Case 116: VbFz1VArith = C116()
    Case 117: VbFz1VArith = C117()
    Case 118: VbFz1VArith = C118()
    Case 119: VbFz1VArith = C119()
    Case 120: VbFz1VArith = C120()
    Case 121: VbFz1VArith = C121()
    Case 122: VbFz1VArith = C122()
    Case 123: VbFz1VArith = C123()
    Case 124: VbFz1VArith = C124()
    Case 125: VbFz1VArith = C125()
    Case 126: VbFz1VArith = C126()
    Case 127: VbFz1VArith = C127()
    Case 128: VbFz1VArith = C128()
    Case 129: VbFz1VArith = C129()
    Case 130: VbFz1VArith = C130()
    Case 131: VbFz1VArith = C131()
    Case 132: VbFz1VArith = C132()
    Case 133: VbFz1VArith = C133()
    Case 134: VbFz1VArith = C134()
    Case 135: VbFz1VArith = C135()
    Case 136: VbFz1VArith = C136()
    Case 137: VbFz1VArith = C137()
    Case 138: VbFz1VArith = C138()
    Case 139: VbFz1VArith = C139()
    Case 140: VbFz1VArith = C140()
    Case 141: VbFz1VArith = C141()
    Case 142: VbFz1VArith = C142()
    Case 143: VbFz1VArith = C143()
    Case 144: VbFz1VArith = C144()
    Case 145: VbFz1VArith = C145()
    Case 146: VbFz1VArith = C146()
    Case 147: VbFz1VArith = C147()
    Case 148: VbFz1VArith = C148()
    Case 149: VbFz1VArith = C149()
    Case 150: VbFz1VArith = C150()
    Case 151: VbFz1VArith = C151()
    Case 152: VbFz1VArith = C152()
    Case 153: VbFz1VArith = C153()
    Case 154: VbFz1VArith = C154()
    Case 155: VbFz1VArith = C155()
    Case 156: VbFz1VArith = C156()
    Case 157: VbFz1VArith = C157()
    Case 158: VbFz1VArith = C158()
    Case 159: VbFz1VArith = C159()
    End Select
    Exit Function
EH:
    VbFz1VArith = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CSng(3.1): s = s & Ap(a)
    b = CSng((-398260.81)): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b Mod a: s = s & Ap(v)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(3): s = s & Ap(a)
    b = CDec("0.0000000000000000000000000001"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b Xor a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = #1/1/2000#: s = s & Ap(a)
    b = CVErr(2042): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-841.46): s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(2): s = s & Ap(a)
    b = CByte(18): s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(2042): s = s & Ap(a)
    b = CInt(16384): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(10): s = s & Ap(a)
    b = CDec(-695): s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b Mod a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b ^ a: s = s & Ap(v)
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "-3": s = s & Ap(a)
    b = CLngLng(CLngLng("-9")): s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "2.5": s = s & Ap(a)
    b = CCur(625800000): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-121399.471): s = s & Ap(a)
    b = Empty: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "  7  ": s = s & Ap(a)
    b = CInt(135): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "&O17": s = s & Ap(a)
    b = CVErr(2042): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLngLng(CLngLng("4294967296")): s = s & Ap(a)
    b = 9.977E-08: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CSng(2492000): s = s & Ap(a)
    b = CDec("1.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(2007): s = s & Ap(a)
    b = 469463.376: s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b + 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec("-2.5"): s = s & Ap(a)
    b = Empty: s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-5.3): s = s & Ap(a)
    b = CLng((-2147483647 - 1)): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b - 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(2042): s = s & Ap(a)
    b = CInt(255): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b \ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(0): s = s & Ap(a)
    b = CLngLng(CLngLng("9223372036854775807")): s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b - CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b \ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b + a: s = s & Ap(v)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-619.26): s = s & Ap(a)
    b = "abc": s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec("0.0000000000000000000000000001"): s = s & Ap(a)
    b = CCur(203): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(10): s = s & Ap(a)
    b = CInt(-32768): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(0): s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = CByte(200): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b \ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b + 1: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = True: s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b ^ a: s = s & Ap(v)
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = True: s = s & Ap(a)
    b = (-26832): s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-915.41): s = s & Ap(a)
    b = CDec("12345678901234567890.123"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b Xor a: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 8.736: s = s & Ap(a)
    b = #12/31/1899#: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b - 1@: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) / b: s = s & Ap(v)
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(2042): s = s & Ap(a)
    b = CLng(-65536): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b + CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-38816): s = s & Ap(a)
    b = Null: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b * CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-3.9): s = s & Ap(a)
    b = CLngLng(CLngLng("1")): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b - CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-24.8981): s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b Xor a: s = s & Ap(v)
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(-1187582873): s = s & Ap(a)
    b = CLngLng(CLngLng("9223372036854775807")): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = ".5": s = s & Ap(a)
    b = 0.0001@: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(-7): s = s & Ap(a)
    b = "$5": s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b * 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b / a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(5): s = s & Ap(a)
    b = "-0": s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "5%": s = s & Ap(a)
    b = CDec("-2.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Empty: s = s & Ap(a)
    b = CInt(-7): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(1): s = s & Ap(a)
    b = CDec("-2.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b >= a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b * 1@: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = True: s = s & Ap(a)
    b = CSng((-17)): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b / a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b + CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(16384): s = s & Ap(a)
    b = CDec(-777): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(1): s = s & Ap(a)
    b = "abc": s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b Xor a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 2.5@: s = s & Ap(a)
    b = CInt(32767): s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(46340): s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b < a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(-7848): s = s & Ap(a)
    b = "  7  ": s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b - 0.5: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec("-2.5"): s = s & Ap(a)
    b = " 12 ": s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b - 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Empty: s = s & Ap(a)
    b = #6:00:00 AM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b * 1@: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) And b: s = s & Ap(v)
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "2.5": s = s & Ap(a)
    b = Null: s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 2.5@: s = s & Ap(a)
    b = "false": s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDate(34): s = s & Ap(a)
    b = CInt(32767): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 339695.057: s = s & Ap(a)
    b = CSng(1884): s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b * 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b < a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec((-80)): s = s & Ap(a)
    b = Empty: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b < a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = CLng(1): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(2147483646): s = s & Ap(a)
    b = Null: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b - 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec(610): s = s & Ap(a)
    b = CVErr(5): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b + 1@: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLngLng(CLngLng("0")): s = s & Ap(a)
    b = #1/1/100#: s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b * a: s = s & Ap(v)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "  7  ": s = s & Ap(a)
    b = Null: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b * CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = CInt(-32768): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLngLng(CLngLng("3037000499")): s = s & Ap(a)
    b = CInt(16384): s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b - 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) / b: s = s & Ap(v)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = Empty: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 0.0001@: s = s & Ap(a)
    b = CLng(65536): s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 1.5@: s = s & Ap(a)
    b = CByte(255): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b >= a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(0): s = s & Ap(a)
    b = CCur(4): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b - CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b + 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b Eqv a: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(181): s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 0.2: s = s & Ap(a)
    b = "$5": s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b + CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec("12345678901234567890.123"): s = s & Ap(a)
    b = "1/2/2000": s = s & Ap(b)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b - 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "2147483648": s = s & Ap(a)
    b = "+4": s = s & Ap(b)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a * b: s = s & Ap(v)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(182): s = s & Ap(a)
    b = 0.0005045000000000001: s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b * 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b - 2147483647: s = s & Ap(v)
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(-238): s = s & Ap(a)
    b = Empty: s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b * 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = #12/31/1899#: s = s & Ap(a)
    b = CDec("79228162514264337593543950335"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b - CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(-32767): s = s & Ap(a)
    b = "1d2": s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(2042): s = s & Ap(a)
    b = CLng(-65536): s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(1000000): s = s & Ap(a)
    b = #1/1/2000#: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b + CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b Imp a: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(7): s = s & Ap(a)
    b = CDec("-2.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b - 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = False: s = s & Ap(a)
    b = Null: s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(5): s = s & Ap(a)
    b = CLng(-65536): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = "0": s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b * a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(5): s = s & Ap(a)
    b = #12/31/1899#: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b + 1@: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) * b: s = s & Ap(v)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "false": s = s & Ap(a)
    b = 6.388000000000001E-09: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b + 1@: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b * 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b < a: s = s & Ap(v)
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "&H10": s = s & Ap(a)
    b = CDec("1.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CSng(23009): s = s & Ap(a)
    b = CLng(-65536): s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b - CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(2147483647): s = s & Ap(a)
    b = #12/31/9999 11:59:59 PM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(-1): s = s & Ap(a)
    b = CSng(6.363E-12): s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b + CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Imp b: s = s & Ap(v)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 2.5@: s = s & Ap(a)
    b = "false": s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b * CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b Mod a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CByte(1): s = s & Ap(a)
    b = #12/31/9999 11:59:59 PM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b / a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Empty: s = s & Ap(a)
    b = 1.5@: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng((-2147483647 - 1)): s = s & Ap(a)
    b = CByte(255): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec("-2.5"): s = s & Ap(a)
    b = CLng(-65536): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec("0.0000000000000000000000000001"): s = s & Ap(a)
    b = CInt(16384): s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b * CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = #2/29/2000 12:00:00 PM#: s = s & Ap(a)
    b = CLng(65536): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(1): s = s & Ap(a)
    b = CDec("12345678901234567890.123"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b + 1@: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = -2.5@: s = s & Ap(a)
    b = DateSerial(2010, 11, 20): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b * 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLngLng(CLngLng("0")): s = s & Ap(a)
    b = CDec("1.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b < a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b * a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 37850000000: s = s & Ap(a)
    b = CVErr(2007): s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b + 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b - 0.5: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "abc": s = s & Ap(a)
    b = CVErr(5): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b Mod a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b * a: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "$5": s = s & Ap(a)
    b = CDec("-2.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLngLng(CLngLng("3")): s = s & Ap(a)
    b = Empty: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Empty: s = s & Ap(a)
    b = "$5": s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(0): s = s & Ap(a)
    b = CDec("1.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = CVErr(5): s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b - 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 0.02: s = s & Ap(a)
    b = #1/1/2000#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b * CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(3): s = s & Ap(a)
    b = CSng(87): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(2007): s = s & Ap(a)
    b = Empty: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec("-2.5"): s = s & Ap(a)
    b = #1/1/2000#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CVErr(5): s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "false": s = s & Ap(a)
    b = 4.5: s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CByte(128): s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b \ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = CInt(1000): s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b - CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "1,234": s = s & Ap(a)
    b = CSng((-78.4)): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b + 0.5: s = s & Ap(v)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-1): s = s & Ap(a)
    b = CCur(1.96E-06): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b * 0.5: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-16.926): s = s & Ap(a)
    b = Null: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b + 0.5: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b + CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) + b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = DateSerial(2052, 10, 9): s = s & Ap(a)
    b = "$5": s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b Eqv a: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(-1775788645): s = s & Ap(a)
    b = CDec("-2.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Mod b: s = s & Ap(v)
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "1E+400": s = s & Ap(a)
    b = (-112.69): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b Mod a: s = s & Ap(v)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CByte(145): s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b Xor a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b - CCur(1): s = s & Ap(v)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "1/2/2000": s = s & Ap(a)
    b = CLngLng(CLngLng("3")): s = s & Ap(b)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(3): s = s & Ap(a)
    b = CDec("-2.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(0): s = s & Ap(a)
    b = CLng(2147483647): s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 2.5@: s = s & Ap(a)
    b = CLng((-2147483647 - 1)): s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b / a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = #12/30/1899#: s = s & Ap(a)
    b = CDate(88): s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b - 1@: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = Empty: s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 207654.097: s = s & Ap(a)
    b = (-2.5): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b ^ a: s = s & Ap(v)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CByte(0): s = s & Ap(a)
    b = CCur(3.968E-06): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b * CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = #12/31/1899#: s = s & Ap(a)
    b = #12/31/1899#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLngLng(CLngLng("10")): s = s & Ap(a)
    b = "5%": s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b * a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(2): s = s & Ap(a)
    b = CInt(182): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CSng(1923): s = s & Ap(a)
    b = CDec("-2.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = #12/31/9999 11:59:59 PM#: s = s & Ap(a)
    b = CLng(1871544557): s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b Xor a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b \ a: s = s & Ap(v)
    Err.Clear: w = a: w = w \ b: s = s & Ap(w)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(181): s = s & Ap(a)
    b = CLngLng(CLngLng("3037000500")): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b + CDec(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec("1.5"): s = s & Ap(a)
    b = CDec("79228162514264337593543950335"): s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CByte(0): s = s & Ap(a)
    b = CCur(505): s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b < a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng((-2147483647 - 1)): s = s & Ap(a)
    b = CCur(100): s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b * 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(2): s = s & Ap(a)
    b = CByte(255): s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b + 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = -2.5@: s = s & Ap(a)
    b = CSng(9.564E-06): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 2373: s = s & Ap(a)
    b = CDec("1.5"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CSng((-588.89)): s = s & Ap(a)
    b = 0.1148: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b - CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a - b: s = s & Ap(v)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(46341): s = s & Ap(a)
    b = CCur(-894): s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec(923.18): s = s & Ap(a)
    b = CDec("79228162514264337593543950335"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: w = a: w = w - b: s = s & Ap(w)
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = "1d2": s = s & Ap(a)
    b = CDec("79228162514264337593543950335"): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b * 2147483647: s = s & Ap(v)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = 6.3: s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b * 1: s = s & Ap(v)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(-1): s = s & Ap(a)
    b = 192271.643: s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Empty: s = s & Ap(a)
    b = Null: s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(2): s = s & Ap(a)
    b = CLngLng(CLngLng("3")): s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a And b: s = s & Ap(v)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Empty: s = s & Ap(a)
    b = 0.00005@: s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Xor b: s = s & Ap(v)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CSng(1E-05): s = s & Ap(a)
    b = CByte(159): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b >= a: s = s & Ap(v)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = #2/29/2000 12:00:00 PM#: s = s & Ap(a)
    b = CDec("0.0000000000000000000000000001"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CByte(206): s = s & Ap(a)
    b = 99820000000000000: s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b Mod a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b - 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b + 0.5: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(3): s = s & Ap(a)
    b = 988.57: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLngLng(CLngLng("4294967296")): s = s & Ap(a)
    b = "  7  ": s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b + CDec(1): s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C149 = s
End Function
Private Function C150() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = Null: s = s & Ap(a)
    b = Null: s = s & Ap(b)
    Err.Clear: v = Empty: v = Abs(a) Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: w = a: w = w + b: s = s & Ap(w)
    C150 = s
End Function
Private Function C151() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CLng(2147483646): s = s & Ap(a)
    b = CByte(254): s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b - 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C151 = s
End Function
Private Function C152() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = 2.5@: s = s & Ap(a)
    b = CDec("0.0000000000000000000000000001"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C152 = s
End Function
Private Function C153() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(236): s = s & Ap(a)
    b = CByte(1): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: w = a: w = w & b: s = s & Ap(w)
    C153 = s
End Function
Private Function C154() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(-7): s = s & Ap(a)
    b = CInt(-7): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b + 1@: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) < b: s = s & Ap(v)
    C154 = s
End Function
Private Function C155() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec(-974): s = s & Ap(a)
    b = CLng(1000000): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: w = a: w = w * b: s = s & Ap(w)
    C155 = s
End Function
Private Function C156() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CDec(0.59): s = s & Ap(a)
    b = #2/29/2000 12:00:00 PM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    C156 = s
End Function
Private Function C157() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = CInt(100): s = s & Ap(a)
    b = CVErr(2042): s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b * CCur(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b - 1: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b \ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C157 = s
End Function
Private Function C158() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = #2/29/2000 12:00:00 PM#: s = s & Ap(a)
    b = (-150550.109): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Abs(a) & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b * 0.5: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: w = a: w = w Mod b: s = s & Ap(w)
    C158 = s
End Function
Private Function C159() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, w As Variant
    a = (-1.5): s = s & Ap(a)
    b = CDec("79228162514264337593543950335"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b * 2147483647: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    C159 = s
End Function
