Attribute VB_Name = "ModVbFz1Arith"
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
Public Function VbFz1Arith(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1Arith = C0()
    Case 1: VbFz1Arith = C1()
    Case 2: VbFz1Arith = C2()
    Case 3: VbFz1Arith = C3()
    Case 4: VbFz1Arith = C4()
    Case 5: VbFz1Arith = C5()
    Case 6: VbFz1Arith = C6()
    Case 7: VbFz1Arith = C7()
    Case 8: VbFz1Arith = C8()
    Case 9: VbFz1Arith = C9()
    Case 10: VbFz1Arith = C10()
    Case 11: VbFz1Arith = C11()
    Case 12: VbFz1Arith = C12()
    Case 13: VbFz1Arith = C13()
    Case 14: VbFz1Arith = C14()
    Case 15: VbFz1Arith = C15()
    Case 16: VbFz1Arith = C16()
    Case 17: VbFz1Arith = C17()
    Case 18: VbFz1Arith = C18()
    Case 19: VbFz1Arith = C19()
    Case 20: VbFz1Arith = C20()
    Case 21: VbFz1Arith = C21()
    Case 22: VbFz1Arith = C22()
    Case 23: VbFz1Arith = C23()
    Case 24: VbFz1Arith = C24()
    Case 25: VbFz1Arith = C25()
    Case 26: VbFz1Arith = C26()
    Case 27: VbFz1Arith = C27()
    Case 28: VbFz1Arith = C28()
    Case 29: VbFz1Arith = C29()
    Case 30: VbFz1Arith = C30()
    Case 31: VbFz1Arith = C31()
    Case 32: VbFz1Arith = C32()
    Case 33: VbFz1Arith = C33()
    Case 34: VbFz1Arith = C34()
    Case 35: VbFz1Arith = C35()
    Case 36: VbFz1Arith = C36()
    Case 37: VbFz1Arith = C37()
    Case 38: VbFz1Arith = C38()
    Case 39: VbFz1Arith = C39()
    Case 40: VbFz1Arith = C40()
    Case 41: VbFz1Arith = C41()
    Case 42: VbFz1Arith = C42()
    Case 43: VbFz1Arith = C43()
    Case 44: VbFz1Arith = C44()
    Case 45: VbFz1Arith = C45()
    Case 46: VbFz1Arith = C46()
    Case 47: VbFz1Arith = C47()
    Case 48: VbFz1Arith = C48()
    Case 49: VbFz1Arith = C49()
    Case 50: VbFz1Arith = C50()
    Case 51: VbFz1Arith = C51()
    Case 52: VbFz1Arith = C52()
    Case 53: VbFz1Arith = C53()
    Case 54: VbFz1Arith = C54()
    Case 55: VbFz1Arith = C55()
    Case 56: VbFz1Arith = C56()
    Case 57: VbFz1Arith = C57()
    Case 58: VbFz1Arith = C58()
    Case 59: VbFz1Arith = C59()
    Case 60: VbFz1Arith = C60()
    Case 61: VbFz1Arith = C61()
    Case 62: VbFz1Arith = C62()
    Case 63: VbFz1Arith = C63()
    Case 64: VbFz1Arith = C64()
    Case 65: VbFz1Arith = C65()
    Case 66: VbFz1Arith = C66()
    Case 67: VbFz1Arith = C67()
    Case 68: VbFz1Arith = C68()
    Case 69: VbFz1Arith = C69()
    Case 70: VbFz1Arith = C70()
    Case 71: VbFz1Arith = C71()
    Case 72: VbFz1Arith = C72()
    Case 73: VbFz1Arith = C73()
    Case 74: VbFz1Arith = C74()
    Case 75: VbFz1Arith = C75()
    Case 76: VbFz1Arith = C76()
    Case 77: VbFz1Arith = C77()
    Case 78: VbFz1Arith = C78()
    Case 79: VbFz1Arith = C79()
    Case 80: VbFz1Arith = C80()
    Case 81: VbFz1Arith = C81()
    Case 82: VbFz1Arith = C82()
    Case 83: VbFz1Arith = C83()
    Case 84: VbFz1Arith = C84()
    Case 85: VbFz1Arith = C85()
    Case 86: VbFz1Arith = C86()
    Case 87: VbFz1Arith = C87()
    Case 88: VbFz1Arith = C88()
    Case 89: VbFz1Arith = C89()
    Case 90: VbFz1Arith = C90()
    Case 91: VbFz1Arith = C91()
    Case 92: VbFz1Arith = C92()
    Case 93: VbFz1Arith = C93()
    Case 94: VbFz1Arith = C94()
    Case 95: VbFz1Arith = C95()
    Case 96: VbFz1Arith = C96()
    Case 97: VbFz1Arith = C97()
    Case 98: VbFz1Arith = C98()
    Case 99: VbFz1Arith = C99()
    Case 100: VbFz1Arith = C100()
    Case 101: VbFz1Arith = C101()
    Case 102: VbFz1Arith = C102()
    Case 103: VbFz1Arith = C103()
    Case 104: VbFz1Arith = C104()
    Case 105: VbFz1Arith = C105()
    Case 106: VbFz1Arith = C106()
    Case 107: VbFz1Arith = C107()
    Case 108: VbFz1Arith = C108()
    Case 109: VbFz1Arith = C109()
    Case 110: VbFz1Arith = C110()
    Case 111: VbFz1Arith = C111()
    Case 112: VbFz1Arith = C112()
    Case 113: VbFz1Arith = C113()
    Case 114: VbFz1Arith = C114()
    Case 115: VbFz1Arith = C115()
    Case 116: VbFz1Arith = C116()
    Case 117: VbFz1Arith = C117()
    Case 118: VbFz1Arith = C118()
    Case 119: VbFz1Arith = C119()
    Case 120: VbFz1Arith = C120()
    Case 121: VbFz1Arith = C121()
    Case 122: VbFz1Arith = C122()
    Case 123: VbFz1Arith = C123()
    Case 124: VbFz1Arith = C124()
    Case 125: VbFz1Arith = C125()
    Case 126: VbFz1Arith = C126()
    Case 127: VbFz1Arith = C127()
    Case 128: VbFz1Arith = C128()
    Case 129: VbFz1Arith = C129()
    Case 130: VbFz1Arith = C130()
    Case 131: VbFz1Arith = C131()
    Case 132: VbFz1Arith = C132()
    Case 133: VbFz1Arith = C133()
    Case 134: VbFz1Arith = C134()
    Case 135: VbFz1Arith = C135()
    Case 136: VbFz1Arith = C136()
    Case 137: VbFz1Arith = C137()
    Case 138: VbFz1Arith = C138()
    Case 139: VbFz1Arith = C139()
    Case 140: VbFz1Arith = C140()
    Case 141: VbFz1Arith = C141()
    Case 142: VbFz1Arith = C142()
    Case 143: VbFz1Arith = C143()
    Case 144: VbFz1Arith = C144()
    Case 145: VbFz1Arith = C145()
    Case 146: VbFz1Arith = C146()
    Case 147: VbFz1Arith = C147()
    Case 148: VbFz1Arith = C148()
    Case 149: VbFz1Arith = C149()
    Case 150: VbFz1Arith = C150()
    Case 151: VbFz1Arith = C151()
    Case 152: VbFz1Arith = C152()
    Case 153: VbFz1Arith = C153()
    Case 154: VbFz1Arith = C154()
    Case 155: VbFz1Arith = C155()
    Case 156: VbFz1Arith = C156()
    Case 157: VbFz1Arith = C157()
    Case 158: VbFz1Arith = C158()
    Case 159: VbFz1Arith = C159()
    End Select
    Exit Function
EH:
    VbFz1Arith = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Byte, r As Byte
    a = 2.5@: s = s & Ap(a)
    b = 115: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Byte, r As Currency
    a = CSng(220299999.99999997): s = s & Ap(a)
    b = 127: s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: r = -a & b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Double, r As Single
    a = 3: s = s & Ap(a)
    b = 6.7: s = s & Ap(b)
    Err.Clear: r = a & b Eqv a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Currency, r As Byte
    a = 0: s = s & Ap(a)
    b = CCur(32699): s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Date, r As Currency
    a = 11315: s = s & Ap(a)
    b = #12/31/9999 11:59:59 PM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = a / b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Long, r As Boolean
    a = CLngLng("3037000500"): s = s & Ap(a)
    b = 2: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As String, r As Currency
    a = 20: s = s & Ap(a)
    b = "abc": s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Date, r As Integer
    a = CSng((-50.33)): s = s & Ap(a)
    b = #6:00:00 AM#: s = s & Ap(b)
    Err.Clear: r = a & b: s = s & Ap(r)
    Err.Clear: r = a & b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: r = Not a Imp b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b < a: s = s & Ap(v)
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Single, r As Single
    a = 127: s = s & Ap(a)
    b = CSng(123456.789): s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: r = a >= b: s = s & Ap(r)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Boolean, r As Single
    a = 254: s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: r = a + b: s = s & Ap(r)
    Err.Clear: r = -a ^ b: s = s & Ap(r)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: r = Not a Imp b: s = s & Ap(r)
    Err.Clear: r = -a Mod b: s = s & Ap(r)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Single, r As Integer
    a = True: s = s & Ap(a)
    b = CSng(20639): s = s & Ap(b)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: r = -a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Integer, r As Boolean
    a = True: s = s & Ap(a)
    b = 1: s = s & Ap(b)
    Err.Clear: r = -a & b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Single, r As Boolean
    a = CCur(-69): s = s & Ap(a)
    b = CSng((-10.5372)): s = s & Ap(b)
    Err.Clear: r = a - b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As String, r As Boolean
    a = 8.237E+18: s = s & Ap(a)
    b = "1.5": s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b + a: s = s & Ap(v)
    Err.Clear: r = a And b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Boolean, r As Byte
    a = "false": s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: r = a Or b: s = s & Ap(r)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Double, r As Long
    a = 209797.403: s = s & Ap(a)
    b = 0.233: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b >= a: s = s & Ap(v)
    Err.Clear: r = a / b: s = s & Ap(r)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As LongLong, r As Long
    a = DateSerial(2008, 5, 19): s = s & Ap(a)
    b = CLngLng("-1"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As String, r As Currency
    a = CCur(0.7): s = s & Ap(a)
    b = "1E+400": s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Long, r As String
    a = 0.00005@: s = s & Ap(a)
    b = (-2147483647 - 1): s = s & Ap(b)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a * b: s = s & Ap(v)
    Err.Clear: r = -a Xor b: s = s & Ap(r)
    Err.Clear: r = a - b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: r = a Eqv b: s = s & Ap(r)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As String, r As Integer
    a = 7: s = s & Ap(a)
    b = "1/2/2000": s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: r = a + b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Boolean, r As Integer
    a = False: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: r = a & b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b >= a: s = s & Ap(v)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Single, r As Single
    a = False: s = s & Ap(a)
    b = CSng(10): s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Boolean, r As Long
    a = 128: s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: r = -a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: r = a + b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: r = -a < b: s = s & Ap(r)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Integer, r As Boolean
    a = CSng(152253.244): s = s & Ap(a)
    b = 256: s = s & Ap(b)
    Err.Clear: r = a ^ b Eqv a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Date, r As Boolean
    a = 0.986: s = s & Ap(a)
    b = CDate(0): s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Boolean, r As String
    a = 32767.5: s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b \ a: s = s & Ap(v)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Boolean, r As Single
    a = 7: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: r = a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Double, r As Date
    a = (-33): s = s & Ap(a)
    b = 27: s = s & Ap(b)
    Err.Clear: r = -a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Boolean, r As Long
    a = 0: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Single, r As Long
    a = False: s = s & Ap(a)
    b = CSng(0.5): s = s & Ap(b)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b * a: s = s & Ap(v)
    Err.Clear: r = a And b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Xor b + a: s = s & Ap(v)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Long, r As Boolean
    a = 922337203685477.5807@: s = s & Ap(a)
    b = 0: s = s & Ap(b)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: r = a & b ^ a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Currency, r As Integer
    a = (-44): s = s & Ap(a)
    b = (-922337203685477.5807@ - 0.0001@): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = a / b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As String, r As Double
    a = CLngLng("3"): s = s & Ap(a)
    b = " 12 ": s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Double, r As Double
    a = "-3": s = s & Ap(a)
    b = 71: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As String, r As Boolean
    a = #2/29/2000 12:00:00 PM#: s = s & Ap(a)
    b = "-3": s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Boolean, r As Integer
    a = "$5": s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a \ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: r = -a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Long, r As Currency
    a = ".5": s = s & Ap(a)
    b = -65536: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b >= a: s = s & Ap(v)
    Err.Clear: r = a \ b: s = s & Ap(r)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Date, r As Currency
    a = "12": s = s & Ap(a)
    b = #2/29/2000 12:00:00 PM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: r = Not a < b: s = s & Ap(r)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Date, r As Long
    a = 4.8: s = s & Ap(a)
    b = #6:00:00 AM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Integer, r As Double
    a = (-5.7): s = s & Ap(a)
    b = 100: s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: r = -a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: r = a And b: s = s & Ap(r)
    Err.Clear: r = a >= b: s = s & Ap(r)
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Single, r As Byte
    a = 2: s = s & Ap(a)
    b = CSng(38873): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b ^ a: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: r = a - b & a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Long, r As Single
    a = 3: s = s & Ap(a)
    b = 2147483647: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: r = Not a Eqv b: s = s & Ap(r)
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Byte, r As Long
    a = "abc": s = s & Ap(a)
    b = 200: s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: r = -a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a - b: s = s & Ap(r)
    Err.Clear: r = a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Long, r As Integer
    a = CLngLng("9223372036854775807"): s = s & Ap(a)
    b = -65597: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Single, r As Date
    a = 1.5@: s = s & Ap(a)
    b = CSng(103607.255): s = s & Ap(b)
    Err.Clear: r = a Eqv b Imp a: s = s & Ap(r)
    Err.Clear: r = -a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Single, r As Long
    a = False: s = s & Ap(a)
    b = CSng((-22150)): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b \ a: s = s & Ap(v)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: r = a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Currency, r As Integer
    a = "2147483648": s = s & Ap(a)
    b = 0.00005@: s = s & Ap(b)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a \ b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As String, r As Boolean
    a = CSng(0.09669): s = s & Ap(a)
    b = "32768": s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b Xor a: s = s & Ap(v)
    Err.Clear: r = -a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Single, r As Double
    a = CLngLng("3"): s = s & Ap(a)
    b = CSng(40): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Single, r As Single
    a = CDate(94): s = s & Ap(a)
    b = CSng((-21)): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Boolean, r As String
    a = 62: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: r = -a + b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Long, r As Date
    a = (-2147483647 - 1): s = s & Ap(a)
    b = 3: s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: r = a & b: s = s & Ap(r)
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Integer, r As Integer
    a = 127: s = s & Ap(a)
    b = -3382: s = s & Ap(b)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a \ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: r = a And b + a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Byte, r As Single
    a = 0.00005@: s = s & Ap(a)
    b = 85: s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Long, r As Long
    a = #1/1/100#: s = s & Ap(a)
    b = (-2147483647 - 1): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b * a: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As String, r As Long
    a = CSng(0.589): s = s & Ap(a)
    b = "1e3": s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: r = -a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b < a: s = s & Ap(v)
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Integer, r As Byte
    a = CSng(1E-300): s = s & Ap(a)
    b = 2: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a / b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Date, r As Byte
    a = 46340: s = s & Ap(a)
    b = #12/31/9999 11:59:59 PM#: s = s & Ap(b)
    Err.Clear: r = a / b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = -a \ b: s = s & Ap(r)
    Err.Clear: r = a - b: s = s & Ap(r)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As String, r As Boolean
    a = 0.00005@: s = s & Ap(a)
    b = "$5": s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: r = a / b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b * a: s = s & Ap(v)
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Long, r As Double
    a = 182: s = s & Ap(a)
    b = 8: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: r = -a Or b: s = s & Ap(r)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b * a: s = s & Ap(v)
    Err.Clear: r = a / b: s = s & Ap(r)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Long, r As Long
    a = CLngLng("9223372036854775807"): s = s & Ap(a)
    b = 65536: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b Mod a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b & a: s = s & Ap(v)
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Boolean, r As Boolean
    a = 128: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a >= b: s = s & Ap(v)
    Err.Clear: r = a And b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As String, r As Double
    a = CSng(0.5): s = s & Ap(a)
    b = "12:30": s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: r = a \ b: s = s & Ap(r)
    Err.Clear: r = a * b + a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Or b >= a: s = s & Ap(v)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Single, r As Byte
    a = (-6.6): s = s & Ap(a)
    b = CSng(634.69): s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b Mod a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Single, r As String
    a = 2147483646: s = s & Ap(a)
    b = CSng((-3463)): s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: r = a \ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b * a: s = s & Ap(v)
    Err.Clear: r = -a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Integer, r As Byte
    a = "2147483648": s = s & Ap(a)
    b = -1: s = s & Ap(b)
    Err.Clear: v = Empty: v = a / b Or a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: r = a \ b Mod a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Boolean, r As String
    a = 7: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: r = Not a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a & b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As LongLong, r As Single
    a = (-80.91): s = s & Ap(a)
    b = CLngLng("1"): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As String, r As Byte
    a = 6.497E+18: s = s & Ap(a)
    b = " ": s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Long, r As Boolean
    a = False: s = s & Ap(a)
    b = 2: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: r = Not a = b: s = s & Ap(r)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Date, r As Currency
    a = 2: s = s & Ap(a)
    b = #1/1/100#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    Err.Clear: r = Not a = b: s = s & Ap(r)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As String, r As Boolean
    a = 7: s = s & Ap(a)
    b = "": s = s & Ap(b)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: r = Not a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Single, r As Double
    a = #12/31/9999 11:59:59 PM#: s = s & Ap(a)
    b = CSng(2.5): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: r = -a + b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Currency, r As Byte
    a = CSng(34549): s = s & Ap(a)
    b = 2.5@: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b >= a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Byte, r As Boolean
    a = 2147483647: s = s & Ap(a)
    b = 127: s = s & Ap(b)
    Err.Clear: r = a + b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a < b = a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Integer, r As Currency
    a = 7: s = s & Ap(a)
    b = -5: s = s & Ap(b)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b / a: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Integer, r As Long
    a = 32766: s = s & Ap(a)
    b = 16384: s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = -a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: r = a Xor b Xor a: s = s & Ap(r)
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Single, r As Single
    a = CDate(8281): s = s & Ap(a)
    b = CSng((-947.4)): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: r = -a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: r = a \ b: s = s & Ap(r)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Date, r As Integer
    a = "1.5": s = s & Ap(a)
    b = #6:00:00 AM#: s = s & Ap(b)
    Err.Clear: r = Not a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Boolean, r As Boolean
    a = 73020000: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b / a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Long, r As Byte
    a = True: s = s & Ap(a)
    b = (-2147483647 - 1): s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b / a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = a & b - a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As LongLong, r As Single
    a = 0: s = s & Ap(a)
    b = CLngLng("-9223372036854775808"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Boolean, r As Integer
    a = DateSerial(2007, 4, 25): s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b ^ a: s = s & Ap(v)
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Date, r As Integer
    a = 10: s = s & Ap(a)
    b = #12/31/1899#: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: r = Not a < b: s = s & Ap(r)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Date, r As Date
    a = -18471323: s = s & Ap(a)
    b = #1/1/100#: s = s & Ap(b)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = -a / b: s = s & Ap(r)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Long, r As Currency
    a = CLngLng("-12040"): s = s & Ap(a)
    b = -10: s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Integer, r As Currency
    a = #12/30/1899#: s = s & Ap(a)
    b = 181: s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b Mod a: s = s & Ap(v)
    Err.Clear: r = a / b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As String, r As Currency
    a = 0.246: s = s & Ap(a)
    b = "&H10": s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Double, r As Double
    a = 181: s = s & Ap(a)
    b = 0.0009888: s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Integer, r As Boolean
    a = CLngLng("0"): s = s & Ap(a)
    b = 100: s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b & a: s = s & Ap(v)
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Double, r As Byte
    a = CSng(3.4E+38): s = s & Ap(a)
    b = (-2.2): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: r = a Eqv b: s = s & Ap(r)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Xor b < a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Date, r As Currency
    a = 128: s = s & Ap(a)
    b = #12/31/9999 11:59:59 PM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b / a: s = s & Ap(v)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As String, r As Boolean
    a = "(5)": s = s & Ap(a)
    b = ".5": s = s & Ap(b)
    Err.Clear: r = a & b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Long, r As Date
    a = CCur(87.92): s = s & Ap(a)
    b = 2: s = s & Ap(b)
    Err.Clear: r = Not a Or b: s = s & Ap(r)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As LongLong, r As String
    a = CLngLng("-1"): s = s & Ap(a)
    b = CLngLng("0"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Boolean, r As Date
    a = CSng((-50)): s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = Not a + b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: r = a >= b / a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Long, r As Integer
    a = 922337203685477.5807@: s = s & Ap(a)
    b = -10: s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Long, r As Byte
    a = 1.5@: s = s & Ap(a)
    b = 0: s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a - b: s = s & Ap(v)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Date, r As Boolean
    a = #12/31/1899#: s = s & Ap(a)
    b = CDate(25901.75371): s = s & Ap(b)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b \ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b * a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Single, r As Currency
    a = (-2147483647 - 1): s = s & Ap(a)
    b = CSng(247.83): s = s & Ap(b)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: r = a & b = a: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a Xor b: s = s & Ap(v)
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As LongLong, r As Single
    a = 248: s = s & Ap(a)
    b = CLngLng("68993"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b ^ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As LongLong, r As Long
    a = 15923: s = s & Ap(a)
    b = CLngLng("3037000500"): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As LongLong, r As Date
    a = (-71): s = s & Ap(a)
    b = CLngLng("9223372036854775807"): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Boolean, r As Long
    a = CLngLng("0"): s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Long, r As Byte
    a = -2.5@: s = s & Ap(a)
    b = 46341: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: r = a + b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b >= a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As String, r As Integer
    a = False: s = s & Ap(a)
    b = "1/2/2000": s = s & Ap(b)
    Err.Clear: r = Not a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b - a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Byte, r As Long
    a = CLngLng("-1"): s = s & Ap(a)
    b = 255: s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Byte, r As Currency
    a = CLngLng("0"): s = s & Ap(a)
    b = 254: s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b Mod a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b * a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a And b: s = s & Ap(v)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Single, r As Boolean
    a = "1/2/2000": s = s & Ap(a)
    b = CSng(159208.08): s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: r = Not a >= b: s = s & Ap(r)
    Err.Clear: r = a / b * a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Integer, r As Date
    a = (-522.89): s = s & Ap(a)
    b = 1000: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Long, r As Double
    a = 2.721E-10: s = s & Ap(a)
    b = -65536: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Double, r As Integer
    a = 0.00005@: s = s & Ap(a)
    b = 0.71: s = s & Ap(b)
    Err.Clear: r = a >= b \ a: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: r = a + b: s = s & Ap(r)
    Err.Clear: r = a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Integer, r As Single
    a = 127: s = s & Ap(a)
    b = 100: s = s & Ap(b)
    Err.Clear: r = a - b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b Eqv a: s = s & Ap(v)
    Err.Clear: r = Not a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As LongLong, r As Integer
    a = -10: s = s & Ap(a)
    b = CLngLng("3"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Date, r As Double
    a = 2: s = s & Ap(a)
    b = #2/29/2000 12:00:00 PM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: r = a * b Imp a: s = s & Ap(r)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As String, r As Byte
    a = CLngLng("-9223372036854775808"): s = s & Ap(a)
    b = "abc": s = s & Ap(b)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b >= a: s = s & Ap(v)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Long, r As Long
    a = "0": s = s & Ap(a)
    b = -1: s = s & Ap(b)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: r = a \ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Double, r As Double
    a = CCur(338): s = s & Ap(a)
    b = 0.542154: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = a / b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Double, r As Long
    a = DateSerial(1910, 5, 15): s = s & Ap(a)
    b = (-36): s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Date, r As Date
    a = True: s = s & Ap(a)
    b = CDate(2): s = s & Ap(b)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b ^ a: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As String, r As Single
    a = 46341: s = s & Ap(a)
    b = "True": s = s & Ap(b)
    Err.Clear: v = Empty: v = a / b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: r = a < b: s = s & Ap(r)
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Double, r As Integer
    a = 65536: s = s & Ap(a)
    b = (-325.31): s = s & Ap(b)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: r = Not a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Currency, r As Long
    a = "3.0": s = s & Ap(a)
    b = 0.0001@: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: r = a = b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: r = -a + b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Boolean, r As Long
    a = CSng(0.1): s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b < a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a + b: s = s & Ap(v)
    Err.Clear: r = -a + b: s = s & Ap(r)
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As String, r As Date
    a = True: s = s & Ap(a)
    b = "1/2/2000": s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b - a: s = s & Ap(v)
    Err.Clear: r = a Xor b & a: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b ^ a: s = s & Ap(v)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Date, r As Double
    a = 254: s = s & Ap(a)
    b = #2/29/2000 12:00:00 PM#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a & b: s = s & Ap(r)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As String, r As Single
    a = CDate(70223): s = s & Ap(a)
    b = "$5": s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: r = a * b >= a: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Eqv b: s = s & Ap(v)
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Integer, r As Boolean
    a = 244: s = s & Ap(a)
    b = -3: s = s & Ap(b)
    Err.Clear: r = -a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Single, r As Boolean
    a = CLngLng("9223372036854775807"): s = s & Ap(a)
    b = CSng(4.3): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b Xor a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Byte, r As Date
    a = (-2147483647 - 1): s = s & Ap(a)
    b = 127: s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Long, r As Boolean
    a = "2147483648": s = s & Ap(a)
    b = -1: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Xor b Xor a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b * a: s = s & Ap(v)
    Err.Clear: r = a Mod b Or a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Single, b As Currency, r As String
    a = CSng(36): s = s & Ap(a)
    b = 0.00005@: s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b = a: s = s & Ap(v)
    Err.Clear: r = a = b Mod a: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b + a: s = s & Ap(v)
    Err.Clear: r = a - b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Long, r As String
    a = CLngLng("0"): s = s & Ap(a)
    b = 127203431: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b Eqv a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Date, r As Double
    a = -10: s = s & Ap(a)
    b = #12/30/1899#: s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a ^ b: s = s & Ap(v)
    Err.Clear: r = a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Single, r As Date
    a = CLngLng("3"): s = s & Ap(a)
    b = CSng(9): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b + a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As LongLong, r As Boolean
    a = CCur((-68.5)): s = s & Ap(a)
    b = CLngLng("3037000499"): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Currency, b As Currency, r As Byte
    a = -2.5@: s = s & Ap(a)
    b = 922337203685477.5807@: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: r = -a - b: s = s & Ap(r)
    Err.Clear: r = a - b: s = s & Ap(r)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: r = a * b: s = s & Ap(r)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Single, r As Double
    a = 25096: s = s & Ap(a)
    b = CSng((-88.6)): s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Boolean, r As Long
    a = -10: s = s & Ap(a)
    b = False: s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = Not a Eqv b: s = s & Ap(r)
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Single, r As String
    a = 7: s = s & Ap(a)
    b = CSng(260020.638): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: r = Not a < b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Byte, b As Currency, r As String
    a = 2: s = s & Ap(a)
    b = CCur((-10387)): s = s & Ap(b)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: r = a >= b >= a: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Date, b As Long, r As Single
    a = #6:00:00 AM#: s = s & Ap(a)
    b = (-2147483647 - 1): s = s & Ap(b)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Integer, r As Double
    a = CLngLng("3037000499"): s = s & Ap(a)
    b = 1: s = s & Ap(b)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Long, r As Integer
    a = CLngLng("3037000499"): s = s & Ap(a)
    b = 2147483646: s = s & Ap(b)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Double, r As Date
    a = "1E+400": s = s & Ap(a)
    b = 9.739000000000001E+17: s = s & Ap(b)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b * a: s = s & Ap(v)
    Err.Clear: r = a & b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a ^ b: s = s & Ap(v)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Long, r As Double
    a = " 12 ": s = s & Ap(a)
    b = -3: s = s & Ap(b)
    Err.Clear: r = -a Imp b: s = s & Ap(r)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: r = a Xor b Or a: s = s & Ap(r)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Long, r As Byte
    a = 1: s = s & Ap(a)
    b = -1: s = s & Ap(b)
    Err.Clear: v = Empty: v = a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Currency, r As Single
    a = "1E+400": s = s & Ap(a)
    b = 0.0001@: s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: r = a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: r = a >= b < a: s = s & Ap(r)
    Err.Clear: r = a And b: s = s & Ap(r)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Integer, r As Single
    a = 342400000000000: s = s & Ap(a)
    b = -1: s = s & Ap(b)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b ^ a: s = s & Ap(v)
    Err.Clear: r = -a + b: s = s & Ap(r)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Single, r As Currency
    a = True: s = s & Ap(a)
    b = CSng(84): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b Eqv a: s = s & Ap(v)
    Err.Clear: r = -a - b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Or b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    C149 = s
End Function
Private Function C150() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Byte, r As Integer
    a = True: s = s & Ap(a)
    b = 164: s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b Imp a: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a / b: s = s & Ap(v)
    Err.Clear: r = -a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = Not a / b: s = s & Ap(v)
    Err.Clear: r = a Or b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C150 = s
End Function
Private Function C151() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Single, r As String
    a = 1000: s = s & Ap(a)
    b = CSng((-268.81)): s = s & Ap(b)
    Err.Clear: v = Empty: v = a Or b \ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Imp b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    C151 = s
End Function
Private Function C152() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Integer, b As Integer, r As Byte
    a = 10: s = s & Ap(a)
    b = 3: s = s & Ap(b)
    Err.Clear: v = Empty: v = -a ^ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b < a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b \ a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    Err.Clear: r = a Xor b: s = s & Ap(r)
    C152 = s
End Function
Private Function C153() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Boolean, b As Integer, r As Date
    a = True: s = s & Ap(a)
    b = -3: s = s & Ap(b)
    Err.Clear: r = a ^ b: s = s & Ap(r)
    Err.Clear: r = a - b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Eqv b = a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: r = a & b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    C153 = s
End Function
Private Function C154() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As LongLong, b As Double, r As Date
    a = CLngLng("3037000499"): s = s & Ap(a)
    b = (-43.71): s = s & Ap(b)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a - b: s = s & Ap(v)
    C154 = s
End Function
Private Function C155() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Boolean, r As String
    a = "1/2/2000": s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Mod b: s = s & Ap(v)
    Err.Clear: r = -a ^ b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a \ b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C155 = s
End Function
Private Function C156() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Long, b As Boolean, r As Currency
    a = -65536: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Mod b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a And b: s = s & Ap(v)
    C156 = s
End Function
Private Function C157() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As String, b As Currency, r As Byte
    a = "+4": s = s & Ap(a)
    b = 2.5@: s = s & Ap(b)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: r = a Eqv b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a Or b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b * a: s = s & Ap(v)
    Err.Clear: r = -a * b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a & b: s = s & Ap(v)
    Err.Clear: r = a - b And a: s = s & Ap(r)
    C157 = s
End Function
Private Function C158() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Boolean, r As Double
    a = (-16598): s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = Not a + b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Or b & a: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a And b: s = s & Ap(v)
    Err.Clear: v = Empty: v = -a Xor b: s = s & Ap(v)
    C158 = s
End Function
Private Function C159() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Double, b As Boolean, r As Currency
    a = 393.5: s = s & Ap(a)
    b = True: s = s & Ap(b)
    Err.Clear: r = a Imp b: s = s & Ap(r)
    Err.Clear: r = a Mod b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a * b: s = s & Ap(v)
    Err.Clear: r = a >= b: s = s & Ap(r)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Mod b And a: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C159 = s
End Function
