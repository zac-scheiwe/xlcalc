Attribute VB_Name = "ModVbFz1Conv"
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
Public Function VbFz1Conv(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1Conv = C0()
    Case 1: VbFz1Conv = C1()
    Case 2: VbFz1Conv = C2()
    Case 3: VbFz1Conv = C3()
    Case 4: VbFz1Conv = C4()
    Case 5: VbFz1Conv = C5()
    Case 6: VbFz1Conv = C6()
    Case 7: VbFz1Conv = C7()
    Case 8: VbFz1Conv = C8()
    Case 9: VbFz1Conv = C9()
    Case 10: VbFz1Conv = C10()
    Case 11: VbFz1Conv = C11()
    Case 12: VbFz1Conv = C12()
    Case 13: VbFz1Conv = C13()
    Case 14: VbFz1Conv = C14()
    Case 15: VbFz1Conv = C15()
    Case 16: VbFz1Conv = C16()
    Case 17: VbFz1Conv = C17()
    Case 18: VbFz1Conv = C18()
    Case 19: VbFz1Conv = C19()
    Case 20: VbFz1Conv = C20()
    Case 21: VbFz1Conv = C21()
    Case 22: VbFz1Conv = C22()
    Case 23: VbFz1Conv = C23()
    Case 24: VbFz1Conv = C24()
    Case 25: VbFz1Conv = C25()
    Case 26: VbFz1Conv = C26()
    Case 27: VbFz1Conv = C27()
    Case 28: VbFz1Conv = C28()
    Case 29: VbFz1Conv = C29()
    Case 30: VbFz1Conv = C30()
    Case 31: VbFz1Conv = C31()
    Case 32: VbFz1Conv = C32()
    Case 33: VbFz1Conv = C33()
    Case 34: VbFz1Conv = C34()
    Case 35: VbFz1Conv = C35()
    Case 36: VbFz1Conv = C36()
    Case 37: VbFz1Conv = C37()
    Case 38: VbFz1Conv = C38()
    Case 39: VbFz1Conv = C39()
    Case 40: VbFz1Conv = C40()
    Case 41: VbFz1Conv = C41()
    Case 42: VbFz1Conv = C42()
    Case 43: VbFz1Conv = C43()
    Case 44: VbFz1Conv = C44()
    Case 45: VbFz1Conv = C45()
    Case 46: VbFz1Conv = C46()
    Case 47: VbFz1Conv = C47()
    Case 48: VbFz1Conv = C48()
    Case 49: VbFz1Conv = C49()
    Case 50: VbFz1Conv = C50()
    Case 51: VbFz1Conv = C51()
    Case 52: VbFz1Conv = C52()
    Case 53: VbFz1Conv = C53()
    Case 54: VbFz1Conv = C54()
    Case 55: VbFz1Conv = C55()
    Case 56: VbFz1Conv = C56()
    Case 57: VbFz1Conv = C57()
    Case 58: VbFz1Conv = C58()
    Case 59: VbFz1Conv = C59()
    Case 60: VbFz1Conv = C60()
    Case 61: VbFz1Conv = C61()
    Case 62: VbFz1Conv = C62()
    Case 63: VbFz1Conv = C63()
    Case 64: VbFz1Conv = C64()
    Case 65: VbFz1Conv = C65()
    Case 66: VbFz1Conv = C66()
    Case 67: VbFz1Conv = C67()
    Case 68: VbFz1Conv = C68()
    Case 69: VbFz1Conv = C69()
    Case 70: VbFz1Conv = C70()
    Case 71: VbFz1Conv = C71()
    Case 72: VbFz1Conv = C72()
    Case 73: VbFz1Conv = C73()
    Case 74: VbFz1Conv = C74()
    Case 75: VbFz1Conv = C75()
    Case 76: VbFz1Conv = C76()
    Case 77: VbFz1Conv = C77()
    Case 78: VbFz1Conv = C78()
    Case 79: VbFz1Conv = C79()
    Case 80: VbFz1Conv = C80()
    Case 81: VbFz1Conv = C81()
    Case 82: VbFz1Conv = C82()
    Case 83: VbFz1Conv = C83()
    Case 84: VbFz1Conv = C84()
    Case 85: VbFz1Conv = C85()
    Case 86: VbFz1Conv = C86()
    Case 87: VbFz1Conv = C87()
    Case 88: VbFz1Conv = C88()
    Case 89: VbFz1Conv = C89()
    Case 90: VbFz1Conv = C90()
    Case 91: VbFz1Conv = C91()
    Case 92: VbFz1Conv = C92()
    Case 93: VbFz1Conv = C93()
    Case 94: VbFz1Conv = C94()
    Case 95: VbFz1Conv = C95()
    Case 96: VbFz1Conv = C96()
    Case 97: VbFz1Conv = C97()
    Case 98: VbFz1Conv = C98()
    Case 99: VbFz1Conv = C99()
    Case 100: VbFz1Conv = C100()
    Case 101: VbFz1Conv = C101()
    Case 102: VbFz1Conv = C102()
    Case 103: VbFz1Conv = C103()
    Case 104: VbFz1Conv = C104()
    Case 105: VbFz1Conv = C105()
    Case 106: VbFz1Conv = C106()
    Case 107: VbFz1Conv = C107()
    Case 108: VbFz1Conv = C108()
    Case 109: VbFz1Conv = C109()
    Case 110: VbFz1Conv = C110()
    Case 111: VbFz1Conv = C111()
    Case 112: VbFz1Conv = C112()
    Case 113: VbFz1Conv = C113()
    Case 114: VbFz1Conv = C114()
    Case 115: VbFz1Conv = C115()
    Case 116: VbFz1Conv = C116()
    Case 117: VbFz1Conv = C117()
    Case 118: VbFz1Conv = C118()
    Case 119: VbFz1Conv = C119()
    Case 120: VbFz1Conv = C120()
    Case 121: VbFz1Conv = C121()
    Case 122: VbFz1Conv = C122()
    Case 123: VbFz1Conv = C123()
    Case 124: VbFz1Conv = C124()
    Case 125: VbFz1Conv = C125()
    Case 126: VbFz1Conv = C126()
    Case 127: VbFz1Conv = C127()
    Case 128: VbFz1Conv = C128()
    Case 129: VbFz1Conv = C129()
    Case 130: VbFz1Conv = C130()
    Case 131: VbFz1Conv = C131()
    Case 132: VbFz1Conv = C132()
    Case 133: VbFz1Conv = C133()
    Case 134: VbFz1Conv = C134()
    Case 135: VbFz1Conv = C135()
    Case 136: VbFz1Conv = C136()
    Case 137: VbFz1Conv = C137()
    Case 138: VbFz1Conv = C138()
    Case 139: VbFz1Conv = C139()
    Case 140: VbFz1Conv = C140()
    Case 141: VbFz1Conv = C141()
    Case 142: VbFz1Conv = C142()
    Case 143: VbFz1Conv = C143()
    Case 144: VbFz1Conv = C144()
    Case 145: VbFz1Conv = C145()
    Case 146: VbFz1Conv = C146()
    Case 147: VbFz1Conv = C147()
    Case 148: VbFz1Conv = C148()
    Case 149: VbFz1Conv = C149()
    Case 150: VbFz1Conv = C150()
    Case 151: VbFz1Conv = C151()
    Case 152: VbFz1Conv = C152()
    Case 153: VbFz1Conv = C153()
    Case 154: VbFz1Conv = C154()
    Case 155: VbFz1Conv = C155()
    Case 156: VbFz1Conv = C156()
    Case 157: VbFz1Conv = C157()
    Case 158: VbFz1Conv = C158()
    Case 159: VbFz1Conv = C159()
    Case 160: VbFz1Conv = C160()
    Case 161: VbFz1Conv = C161()
    Case 162: VbFz1Conv = C162()
    Case 163: VbFz1Conv = C163()
    Case 164: VbFz1Conv = C164()
    Case 165: VbFz1Conv = C165()
    Case 166: VbFz1Conv = C166()
    Case 167: VbFz1Conv = C167()
    Case 168: VbFz1Conv = C168()
    Case 169: VbFz1Conv = C169()
    End Select
    Exit Function
EH:
    VbFz1Conv = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(1000)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-258541.237)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(32766)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(-8)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(1188123933)
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(0)
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(2147483646)
    Err.Clear: v = Empty: v = Val("1,5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-31.129))
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(1)
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDate(7378)
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(100)
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDate(46531.58)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = Int( 18.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-7)
    Err.Clear: v = Empty: v = Val("&B101"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng(29659)
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = CInt( 20.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.00005@
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-27.7)
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("12345678901234567890.123")
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-1)
    Err.Clear: v = Empty: v = CLng(a + -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #12/30/1899#
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-1122))
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2.5"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 0.008279
    Err.Clear: v = Empty: v = Int( (-11.75)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-1)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 604.64
    Err.Clear: v = Empty: v = CByte(a + -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(65536)
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(7)
    Err.Clear: v = Empty: v = Round( (-0.4999999999999999)): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(0)
    Err.Clear: v = Empty: v = Fix(- 20.75): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(128)
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(1000000)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(0)
    Err.Clear: v = Empty: v = Round(- 5.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(5)
    Err.Clear: v = Empty: v = Fix(- 5.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " "
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CSng(448400000000)
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = CByte(a * 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 8.446E-11
    Err.Clear: v = Empty: v = Val("1,5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-312.26)
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDate(43)
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = CByte(a * -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = Val("1e400"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = CInt(a - 2.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = 10000000000000000
    Err.Clear: v = Empty: v = Val("1e-400"): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = Val("1 e 2"): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CCur(-158)
    Err.Clear: v = Empty: v = CByte(a + -0.5): s = s & Ap(v)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = Round(a, 15): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("1 e 2"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 4125
    Err.Clear: v = Empty: v = Val("-&H10"): s = s & Ap(v)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(168)
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #1/1/2000#
    Err.Clear: v = Empty: v = Round(a): s = s & Ap(v)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(11939)
    Err.Clear: v = Empty: v = Val("1e3"): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 9.059E-09
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CByte(a * -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(8.747E+18)
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-41)
    Err.Clear: v = Empty: v = CLng(a - 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2.5"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = CByte(a + -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 439900000
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("12345678901234567890.123")
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-271)
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("3037000499"))
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = CInt(a + 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 2.336
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = DateSerial(2057, 7, 8)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CLng(a + 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = -2.5@
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 1E-05
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-29054)
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-922337203685477.5807@ - 0.0001@)
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-20261))
    Err.Clear: v = Empty: v = Int( 4.5): s = s & Ap(v)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Int( 12.75): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(181)
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(2147483647)
    Err.Clear: v = Empty: v = Round(a, 22): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(46341)
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 1.5@
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-71))
    Err.Clear: v = Empty: v = Round(a): s = s & Ap(v)
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 2354000000000000
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(7)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " "
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-0"
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(32767)
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(1)
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = CCur(a * 2.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-32768)
    Err.Clear: v = Empty: v = Fix( 6.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = Val("&B101"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CCur(a - 2.5): s = s & Ap(v)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-1)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng(2147483647.5)
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(127)
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(181)
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #1/1/100#
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("0"))
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 0.05947
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = Val("1,5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(182)
    Err.Clear: v = Empty: v = CInt(- (-13.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = Round(- (-19.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = 123456.789
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2.5"
    Err.Clear: v = Empty: v = Round(- 8.75): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = CLng(a - -0.5): s = s & Ap(v)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = Int( 4.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2042)
    Err.Clear: v = Empty: v = Round(a, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-0"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("-5875"))
    Err.Clear: v = Empty: v = Val("&O777"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(5)
    Err.Clear: v = Empty: v = CInt( 19.5): s = s & Ap(v)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(-7)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(127)
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(-65536)
    Err.Clear: v = Empty: v = Val("&B101"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 9.9
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CInt( (-1.5)): s = s & Ap(v)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 0.0001@
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-55)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-57.09)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 7.953
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 0.00005@
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-32767)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = Round(a, 1): s = s & Ap(v)
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(0.441345)
    Err.Clear: v = Empty: v = Fix(- (-7.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(10)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("&HFFFF"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng((-2147483647 - 1))
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(10)
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(5)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = Int(- (-15.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(46341)
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2007)
    Err.Clear: v = Empty: v = Round( (-12.5)): s = s & Ap(v)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng((-2147483647 - 1))
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("12345678901234567890.123")
    Err.Clear: v = Empty: v = CInt(a + -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(255)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CSng((-35292))
    Err.Clear: v = Empty: v = Val("1e400"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(1000)
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = CCur(a - 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = Val("1 e 2"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CCur(-395)
    Err.Clear: v = Empty: v = Fix(- (-9.25)): s = s & Ap(v)
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("1e-400"): s = s & Ap(v)
    Err.Clear: a = Empty: a = 65210000
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("&HFFFF"): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-65536)
    Err.Clear: v = Empty: v = CByte(a * 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng(1000000000000000)
    Err.Clear: v = Empty: v = CLng(a + -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(255)
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(2147483646)
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(4.434999999999999E-07)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = Fix(- 13.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Round(a, -1): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("12345678901234567890.123")
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(5)
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = CByte(a + 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = DateSerial(2037, 11, 6)
    Err.Clear: v = Empty: v = CLng(a * 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng(5.041E-10)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CInt(a + 2.5): s = s & Ap(v)
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 1000000000000000
    Err.Clear: v = Empty: v = Int( 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-0.4))
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(65536)
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = Val("  -.5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = CLng(a - 0.5): s = s & Ap(v)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Round(a, 15): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("1 e 2"): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-1)
    Err.Clear: v = Empty: v = Val("1 e 2"): s = s & Ap(v)
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.00005@
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(5)
    Err.Clear: v = Empty: v = Round( 20.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-1)
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("5"))
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(128)
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CCur(-228)
    Err.Clear: v = Empty: v = CLng( 11.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("12345678901234567890.123")
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CVErr(5)
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = Fix(- 2.75): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = Round(a, 22): s = s & Ap(v)
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = (-66.61)
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(10)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(1000)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "0"
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #12/31/1899#
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(0)
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CCur(1E+300)
    Err.Clear: v = Empty: v = Fix( 20.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 189.36
    Err.Clear: v = Empty: v = CCur(a - 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-10)
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = CInt(a * 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = Val("  1 2 3"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-1)
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-8937)
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-13881)
    Err.Clear: v = Empty: v = Val("  -.5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-922337203685477.5807@ - 0.0001@)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-5.2))
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CInt(- 5.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(32766)
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "-0"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CByte(a - 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-1)
    Err.Clear: v = Empty: v = Int( (-11.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDate(1662.73)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = Val("  1 2 3"): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = Round(a, -1): s = s & Ap(v)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = #1/1/2000#
    Err.Clear: v = Empty: v = CCur(a + 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(182)
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CVErr(2007)
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Round(a, 15): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-5)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("12345678901234567890.123")
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDate(68092.851)
    Err.Clear: v = Empty: v = CLng(a * -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = CCur(a + 0.5): s = s & Ap(v)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(128)
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(32766)
    Err.Clear: v = Empty: v = Int(- (-16.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = 6.8
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(0)
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-36)
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-379039.422))
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = Int( 11.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-1)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(-65536)
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(10)
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = CLng(a * 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Int( 18.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CCur(a + 1.5): s = s & Ap(v)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(10)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CCur(-595)
    Err.Clear: v = Empty: v = CCur(a * -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = 15069
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-5)
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLngLng(CLngLng("3037000500"))
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(127)
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CCur(275)
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = Round(a, 22): s = s & Ap(v)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = Round(a, 15): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.05
    Err.Clear: v = Empty: v = Int(- (-0.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = 35086
    Err.Clear: v = Empty: v = Int( (-8.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "-0"
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = Round(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = Val("  1 2 3"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = Round(a, 15): s = s & Ap(v)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 143751.893
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(3)
    Err.Clear: v = Empty: v = Round(- 9.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-1)
    Err.Clear: v = Empty: v = Round( (-9.75)): s = s & Ap(v)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CVErr(2007)
    Err.Clear: v = Empty: v = Val("1e400"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(10)
    Err.Clear: v = Empty: v = Val("  -.5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("9"))
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(255)
    Err.Clear: v = Empty: v = Round(a, 15): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CVErr(2042)
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(9)
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Fix(- (-17.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(256)
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CByte(a + -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2007)
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("3"))
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Val("&HFFFF"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = CInt(- (-4.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = CLng(a - 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = DateSerial(2078, 7, 2)
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(695808013)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(1)
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(254)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " "
    Err.Clear: v = Empty: v = Val("  -.5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(1.377E-05)
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.2
    Err.Clear: v = Empty: v = Val("&HFFFF"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CCur(0.09897)
    Err.Clear: v = Empty: v = Round(a, 22): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CSng((-807.13))
    Err.Clear: v = Empty: v = Val("-&H10"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = Val("1e400"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 922337203685477.5807@
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2007)
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(533747231)
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(1)
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng(19.24)
    Err.Clear: v = Empty: v = Round( (-11.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(10)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("(5)"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = CLng(a + 2.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(65536)
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 2.5
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(16384)
    Err.Clear: v = Empty: v = CLng(a * 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng((-2147483647 - 1))
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = Round(a, -1): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = CInt(a - -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(100)
    Err.Clear: v = Empty: v = Val("-&H10"): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-2.1))
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(255)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(-346)
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(-32767)
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = Round(a, 2): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(32766)
    Err.Clear: v = Empty: v = CInt(a - 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(7)
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-65536)
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-32767)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(7)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = CLng(a - 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = CInt( (-10.75)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2.5"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = CCur(a - -0.5): s = s & Ap(v)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CSng((-38.55))
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(100)
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("1"))
    Err.Clear: v = Empty: v = Val("1 e 2"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(1000)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Val("1e400"): s = s & Ap(v)
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CInt(- 8.75): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(3)
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2.5"
    Err.Clear: v = Empty: v = Fix(- 15.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CByte(a - 1.5): s = s & Ap(v)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 1.5
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(1000)
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #12/30/1899#
    Err.Clear: v = Empty: v = CCur(a + 2.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-206.25))
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(2)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng((-2147483647 - 1))
    Err.Clear: v = Empty: v = CLng(a * 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = Round(a, 15): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = Int(- 6.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.0001@
    Err.Clear: v = Empty: v = Val("1,5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 38900
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng(16956)
    Err.Clear: v = Empty: v = CInt( (-9.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(254)
    Err.Clear: v = Empty: v = Val("  1 2 3"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(3)
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(1000)
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = Int(- 0.5000000000000001): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CVErr(5)
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(565)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = Round(a, 1): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(7)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(10)
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = Val("1e3"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(255)
    Err.Clear: v = Empty: v = Int(- 12.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(2)
    Err.Clear: v = Empty: v = CInt(- (-10.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = (-20.9)
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(46340)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = CInt(a * 0.25): s = s & Ap(v)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 2.5@
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(7)
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = CCur(a * 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = Fix(- (-15.5)): s = s & Ap(v)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = " "
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = CCur(a - 2.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("&HFFFF"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CByte(a + -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(2)
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLngLng(CLngLng("-1"))
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 1.5@
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 1.5@
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("3"))
    Err.Clear: v = Empty: v = Val("1/2/2000"): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = (-3.4)
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = CLng(- 7.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(3)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = Round(a, 22): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #1/1/2000#
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDate(55143.008)
    Err.Clear: v = Empty: v = CInt(- (-12.75)): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(256)
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = CInt(a + 2.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(-575149363)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-10)
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-1E+300)
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-21600)
    Err.Clear: v = Empty: v = Round(a, 1): s = s & Ap(v)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = CInt(- 2.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(32767)
    Err.Clear: v = Empty: v = CCur(a - 2.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = Int(- 18.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = True
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(0)
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(32766)
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(32766)
    Err.Clear: v = Empty: v = Val("1,5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = 922337203685477.5807@
    Err.Clear: v = Empty: v = Int( (-9.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0"
    Err.Clear: v = Empty: v = Val("+4"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng(0.3446)
    Err.Clear: v = Empty: v = Round(a, 2): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-10)
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-96.8)
    Err.Clear: v = Empty: v = CInt(- 15.75): s = s & Ap(v)
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2042)
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " "
    Err.Clear: v = Empty: v = Val("&HFFFFFFFF"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng((-2147483647 - 1))
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(-112)
    Err.Clear: v = Empty: v = Val("1e400"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(200)
    Err.Clear: v = Empty: v = Round(a, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = 1000000000000000
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = Round(a, -1): s = s & Ap(v)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = (-25192)
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2042)
    Err.Clear: v = Empty: v = CByte(a + 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CLng(- 10.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = 96
    Err.Clear: v = Empty: v = Val("1,5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CCur(9.5)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec(500.46)
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.005
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-8.6)
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CLng(a + 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 2.5@
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(-32236)
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "2.5"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " 12 "
    Err.Clear: v = Empty: v = Round(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CByte(a + 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2042)
    Err.Clear: v = Empty: v = CInt(a - 0.25): s = s & Ap(v)
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CCur((-86))
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Int(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CInt(a * 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = Val("1 e 2"): s = s & Ap(v)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CCur(769)
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(255)
    Err.Clear: v = Empty: v = CCur(a + 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = CLng(a * 2.5): s = s & Ap(v)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(256)
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("-1"))
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = Round(a, 22): s = s & Ap(v)
    Err.Clear: a = Empty: a = #12/31/9999 11:59:59 PM#
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(1000)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(-341)
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(2147483646)
    Err.Clear: v = Empty: v = CCur(a * 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = " "
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = Val("&HFFFF"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0"
    Err.Clear: v = Empty: v = Fix( (-15.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("5"))
    Err.Clear: v = Empty: v = Round(a, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0"
    Err.Clear: v = Empty: v = Round( (-2.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLngLng(CLngLng("-1"))
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = CLng( (-6.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(127)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(46340)
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("-&H10"): s = s & Ap(v)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = (-1E+300)
    Err.Clear: v = Empty: v = CInt( 4.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2.5"
    Err.Clear: v = Empty: v = CCur(a * -0.5): s = s & Ap(v)
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "32768"
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = -2.5@
    Err.Clear: v = Empty: v = CLng(a * 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.555
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.37116
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = CLng( (-2.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(6.088000000000001E-05)
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-0"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng((-27729))
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = (-110379.919)
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = Val("&HFFFFFFFF"): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-41029.609)
    Err.Clear: v = Empty: v = Fix( 6.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("0"))
    Err.Clear: v = Empty: v = CInt(- 18.25): s = s & Ap(v)
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = Val("&O777"): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(0)
    Err.Clear: v = Empty: v = CByte(a - 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(64)
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Val("1 e 2"): s = s & Ap(v)
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "-0"
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = CLng(a + -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #12/31/9999 11:59:59 PM#
    Err.Clear: v = Empty: v = CCur(a - 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-32767)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(2)
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = Round(a, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLng(65536)
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Round( (-7.25)): s = s & Ap(v)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #12/31/1899#
    Err.Clear: v = Empty: v = CInt(a * 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(255)
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "  7  "
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-7)
    Err.Clear: v = Empty: v = Val("1.5e2x"): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(50279999999.99999)
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #6:00:00 AM#
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = Val("  -.5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2042)
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-32768)
    Err.Clear: v = Empty: v = TypeName(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Round(a, -1): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = Round(a, 2): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1d2"
    Err.Clear: v = Empty: v = Val("abc"): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = Round(a, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&H10"
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(10)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng((-2147483647 - 1))
    Err.Clear: v = Empty: v = Round(a, 2): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(5)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "+4"
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "abc"
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(19122)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CCur(a - 2.5): s = s & Ap(v)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Fix(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = CInt(- 10.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.0001@
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(607.12)
    Err.Clear: v = Empty: v = CCur(a - -0.5): s = s & Ap(v)
    C149 = s
End Function
Private Function C150() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(-32767)
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 37829.206
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("79228162514264337593543950335")
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-92)
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    C150 = s
End Function
Private Function C151() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(32766)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = DateSerial(2001, 12, 28)
    Err.Clear: v = Empty: v = Round(a, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = CInt( 4.75): s = s & Ap(v)
    C151 = s
End Function
Private Function C152() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = ""
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CSng(4.834E-09)
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(127)
    Err.Clear: v = Empty: v = Round(a, 3): s = s & Ap(v)
    C152 = s
End Function
Private Function C153() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLngLng(CLngLng("3037000500"))
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("1"))
    Err.Clear: v = Empty: v = CInt(a - 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.582
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 9439
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1,234"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    C153 = s
End Function
Private Function C154() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CLngLng(CLngLng("3037000499"))
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(192)
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = CCur(a * 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec(-247)
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    C154 = s
End Function
Private Function C155() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "false"
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(1)
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = CSng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(3)
    Err.Clear: v = Empty: v = Round(a, 15): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("9"))
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C155 = s
End Function
Private Function C156() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 257600
    Err.Clear: v = Empty: v = Round(a, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = 2.5@
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-22)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    C156 = s
End Function
Private Function C157() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Null
    Err.Clear: v = Empty: v = CInt(a * 2.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("12345678901234567890.123")
    Err.Clear: v = Empty: v = Round( 6.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "&O17"
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    C157 = s
End Function
Private Function C158() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "0"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2042)
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = Hex$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    C158 = s
End Function
Private Function C159() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 32767.5
    Err.Clear: v = Empty: v = Fix(- (-12.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "-3"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #12/30/1899#
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("-9223372036854775808"))
    Err.Clear: v = Empty: v = CByte(a + 0.25): s = s & Ap(v)
    C159 = s
End Function
Private Function C160() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = IsNumeric(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12"
    Err.Clear: v = Empty: v = Val("12abc34"): s = s & Ap(v)
    Err.Clear: a = Empty: a = 1.5@
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "(5)"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    C160 = s
End Function
Private Function C161() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = CCur(a - 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = 1.4
    Err.Clear: v = Empty: v = Val("  -.5"): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-922337203685477.5807@ - 0.0001@)
    Err.Clear: v = Empty: v = CCur(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = -2.5@
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2.5"
    Err.Clear: v = Empty: v = CLngLng(a): s = s & Ap(v)
    C161 = s
End Function
Private Function C162() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = False
    Err.Clear: v = Empty: v = CLng( (-14.5)): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = IsDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLngLng(CLngLng("9223372036854775807"))
    Err.Clear: v = Empty: v = CByte(a * 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(10)
    Err.Clear: v = Empty: v = CLng(- 18.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "3.0"
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    C162 = s
End Function
Private Function C163() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = 529.02
    Err.Clear: v = Empty: v = CInt(a - 0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = Str(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = CDbl(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(100)
    Err.Clear: v = Empty: v = CStr(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CLng(0)
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    C163 = s
End Function
Private Function C164() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = DateSerial(2098, 12, 12)
    Err.Clear: v = Empty: v = Oct(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("-2.5")
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = Hex(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = #2/29/2000 12:00:00 PM#
    Err.Clear: v = Empty: v = CLng(- (-7.25)): s = s & Ap(v)
    Err.Clear: a = Empty: a = "12:30"
    Err.Clear: v = Empty: v = CInt(a): s = s & Ap(v)
    C164 = s
End Function
Private Function C165() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CByte(7)
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("1.5")
    Err.Clear: v = Empty: v = CDate(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "5%"
    Err.Clear: v = Empty: v = CInt(- 4.75): s = s & Ap(v)
    C165 = s
End Function
Private Function C166() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = ".5"
    Err.Clear: v = Empty: v = CDec(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1e3"
    Err.Clear: v = Empty: v = Str$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(-115)
    Err.Clear: v = Empty: v = Oct$(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CInt(2)
    Err.Clear: v = Empty: v = Sgn(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0"
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    C166 = s
End Function
Private Function C167() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = (-922337203685477.5807@ - 0.0001@)
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = VarType(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CDec("0.0000000000000000000000000001")
    Err.Clear: v = Empty: v = CByte(a * 0.25): s = s & Ap(v)
    Err.Clear: a = Empty: a = "0x10"
    Err.Clear: v = Empty: v = CLng(a - 1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "$5"
    Err.Clear: v = Empty: v = CBool(a): s = s & Ap(v)
    C167 = s
End Function
Private Function C168() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = CInt(256)
    Err.Clear: v = Empty: v = CVar(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = CVErr(2042)
    Err.Clear: v = Empty: v = CLng(a - -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = (-1.5)
    Err.Clear: v = Empty: v = Len(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = 0.34869
    Err.Clear: v = Empty: v = CLng(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1.5"
    Err.Clear: v = Empty: v = Val("-&H10"): s = s & Ap(v)
    C168 = s
End Function
Private Function C169() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant
    Err.Clear: a = Empty: a = Empty
    Err.Clear: v = Empty: v = CCur(a * -0.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = "2147483648"
    Err.Clear: v = Empty: v = Round(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1/2/2000"
    Err.Clear: v = Empty: v = Abs(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "True"
    Err.Clear: v = Empty: v = CByte(a): s = s & Ap(v)
    Err.Clear: a = Empty: a = "1E+400"
    Err.Clear: v = Empty: v = CInt(a - 2.5): s = s & Ap(v)
    C169 = s
End Function
