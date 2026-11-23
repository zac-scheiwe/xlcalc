Attribute VB_Name = "ModVbFz1CmpT"
Option Compare Text
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
Public Function VbFz1CmpT(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1CmpT = C0()
    Case 1: VbFz1CmpT = C1()
    Case 2: VbFz1CmpT = C2()
    Case 3: VbFz1CmpT = C3()
    Case 4: VbFz1CmpT = C4()
    Case 5: VbFz1CmpT = C5()
    Case 6: VbFz1CmpT = C6()
    Case 7: VbFz1CmpT = C7()
    Case 8: VbFz1CmpT = C8()
    Case 9: VbFz1CmpT = C9()
    Case 10: VbFz1CmpT = C10()
    Case 11: VbFz1CmpT = C11()
    Case 12: VbFz1CmpT = C12()
    Case 13: VbFz1CmpT = C13()
    Case 14: VbFz1CmpT = C14()
    Case 15: VbFz1CmpT = C15()
    Case 16: VbFz1CmpT = C16()
    Case 17: VbFz1CmpT = C17()
    Case 18: VbFz1CmpT = C18()
    Case 19: VbFz1CmpT = C19()
    Case 20: VbFz1CmpT = C20()
    Case 21: VbFz1CmpT = C21()
    Case 22: VbFz1CmpT = C22()
    Case 23: VbFz1CmpT = C23()
    Case 24: VbFz1CmpT = C24()
    Case 25: VbFz1CmpT = C25()
    Case 26: VbFz1CmpT = C26()
    Case 27: VbFz1CmpT = C27()
    Case 28: VbFz1CmpT = C28()
    Case 29: VbFz1CmpT = C29()
    Case 30: VbFz1CmpT = C30()
    Case 31: VbFz1CmpT = C31()
    Case 32: VbFz1CmpT = C32()
    Case 33: VbFz1CmpT = C33()
    Case 34: VbFz1CmpT = C34()
    Case 35: VbFz1CmpT = C35()
    Case 36: VbFz1CmpT = C36()
    Case 37: VbFz1CmpT = C37()
    Case 38: VbFz1CmpT = C38()
    Case 39: VbFz1CmpT = C39()
    Case 40: VbFz1CmpT = C40()
    Case 41: VbFz1CmpT = C41()
    Case 42: VbFz1CmpT = C42()
    Case 43: VbFz1CmpT = C43()
    Case 44: VbFz1CmpT = C44()
    Case 45: VbFz1CmpT = C45()
    Case 46: VbFz1CmpT = C46()
    Case 47: VbFz1CmpT = C47()
    Case 48: VbFz1CmpT = C48()
    Case 49: VbFz1CmpT = C49()
    Case 50: VbFz1CmpT = C50()
    Case 51: VbFz1CmpT = C51()
    Case 52: VbFz1CmpT = C52()
    Case 53: VbFz1CmpT = C53()
    Case 54: VbFz1CmpT = C54()
    Case 55: VbFz1CmpT = C55()
    Case 56: VbFz1CmpT = C56()
    Case 57: VbFz1CmpT = C57()
    Case 58: VbFz1CmpT = C58()
    Case 59: VbFz1CmpT = C59()
    Case 60: VbFz1CmpT = C60()
    Case 61: VbFz1CmpT = C61()
    Case 62: VbFz1CmpT = C62()
    Case 63: VbFz1CmpT = C63()
    Case 64: VbFz1CmpT = C64()
    Case 65: VbFz1CmpT = C65()
    Case 66: VbFz1CmpT = C66()
    Case 67: VbFz1CmpT = C67()
    Case 68: VbFz1CmpT = C68()
    Case 69: VbFz1CmpT = C69()
    Case 70: VbFz1CmpT = C70()
    Case 71: VbFz1CmpT = C71()
    Case 72: VbFz1CmpT = C72()
    Case 73: VbFz1CmpT = C73()
    Case 74: VbFz1CmpT = C74()
    Case 75: VbFz1CmpT = C75()
    Case 76: VbFz1CmpT = C76()
    Case 77: VbFz1CmpT = C77()
    Case 78: VbFz1CmpT = C78()
    Case 79: VbFz1CmpT = C79()
    Case 80: VbFz1CmpT = C80()
    Case 81: VbFz1CmpT = C81()
    Case 82: VbFz1CmpT = C82()
    Case 83: VbFz1CmpT = C83()
    Case 84: VbFz1CmpT = C84()
    Case 85: VbFz1CmpT = C85()
    Case 86: VbFz1CmpT = C86()
    Case 87: VbFz1CmpT = C87()
    Case 88: VbFz1CmpT = C88()
    Case 89: VbFz1CmpT = C89()
    Case 90: VbFz1CmpT = C90()
    Case 91: VbFz1CmpT = C91()
    Case 92: VbFz1CmpT = C92()
    Case 93: VbFz1CmpT = C93()
    Case 94: VbFz1CmpT = C94()
    Case 95: VbFz1CmpT = C95()
    Case 96: VbFz1CmpT = C96()
    Case 97: VbFz1CmpT = C97()
    Case 98: VbFz1CmpT = C98()
    Case 99: VbFz1CmpT = C99()
    Case 100: VbFz1CmpT = C100()
    Case 101: VbFz1CmpT = C101()
    Case 102: VbFz1CmpT = C102()
    Case 103: VbFz1CmpT = C103()
    Case 104: VbFz1CmpT = C104()
    Case 105: VbFz1CmpT = C105()
    Case 106: VbFz1CmpT = C106()
    Case 107: VbFz1CmpT = C107()
    Case 108: VbFz1CmpT = C108()
    Case 109: VbFz1CmpT = C109()
    Case 110: VbFz1CmpT = C110()
    Case 111: VbFz1CmpT = C111()
    Case 112: VbFz1CmpT = C112()
    Case 113: VbFz1CmpT = C113()
    Case 114: VbFz1CmpT = C114()
    Case 115: VbFz1CmpT = C115()
    Case 116: VbFz1CmpT = C116()
    Case 117: VbFz1CmpT = C117()
    Case 118: VbFz1CmpT = C118()
    Case 119: VbFz1CmpT = C119()
    Case 120: VbFz1CmpT = C120()
    Case 121: VbFz1CmpT = C121()
    Case 122: VbFz1CmpT = C122()
    Case 123: VbFz1CmpT = C123()
    Case 124: VbFz1CmpT = C124()
    Case 125: VbFz1CmpT = C125()
    Case 126: VbFz1CmpT = C126()
    Case 127: VbFz1CmpT = C127()
    Case 128: VbFz1CmpT = C128()
    Case 129: VbFz1CmpT = C129()
    Case 130: VbFz1CmpT = C130()
    Case 131: VbFz1CmpT = C131()
    Case 132: VbFz1CmpT = C132()
    Case 133: VbFz1CmpT = C133()
    Case 134: VbFz1CmpT = C134()
    Case 135: VbFz1CmpT = C135()
    Case 136: VbFz1CmpT = C136()
    Case 137: VbFz1CmpT = C137()
    Case 138: VbFz1CmpT = C138()
    Case 139: VbFz1CmpT = C139()
    Case 140: VbFz1CmpT = C140()
    Case 141: VbFz1CmpT = C141()
    Case 142: VbFz1CmpT = C142()
    Case 143: VbFz1CmpT = C143()
    Case 144: VbFz1CmpT = C144()
    Case 145: VbFz1CmpT = C145()
    Case 146: VbFz1CmpT = C146()
    Case 147: VbFz1CmpT = C147()
    Case 148: VbFz1CmpT = C148()
    Case 149: VbFz1CmpT = C149()
    End Select
    Exit Function
EH:
    VbFz1CmpT = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "9": b = "9": t = "-1": u = "b"
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "2": b = "A1": t = "a1": u = "-1"
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "True": b = "?": t = "ae": u = "aBc"
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "abc": b = "E": t = (ChrW(233)): u = "ae"
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "a*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[*]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "(5)": b = "SS": t = "b": u = "x-y"
    Err.Clear: v = Empty: v = a Like "[!0-9]?": s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[!0-9]?": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "2": b = "ss": t = "[": u = "a1"
    Err.Clear: v = Empty: v = a Like "": s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like (ChrW(233) & "*"): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "-3": b = Empty: t = "1": u = "?"
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[*]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*b*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "SS": b = "True": t = "1.5": u = "2"
    Err.Clear: v = Empty: v = t Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "a*": s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[!0-9]?": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) And (t = u): s = s & Ap(v)
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = True: b = CDec(616): t = "A1": u = "2"
    Err.Clear: v = Empty: v = (a > b) Xor (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like (ChrW(233) & "*"): s = s & Ap(v)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = Empty: b = (ChrW(198)): t = "2": u = ""
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = (ChrW(223)): b = "3.0": t = "True": u = ""
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "z": b = "ABC": t = "9": u = "#"
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Or (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "": b = "B": t = "*": u = "B"
    Err.Clear: v = Empty: v = a Like "?*?": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ss": b = " ": t = " ": u = "ae"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "": b = "e": t = "A1": u = "E"
    Err.Clear: v = Empty: v = (a > b) And (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*[0-9]": s = s & Ap(v)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "?": b = "abd": t = "ab": u = "Z"
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = (ChrW(223)): b = "aBc": t = "?": u = "?"
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = " ": b = "b": t = "#": u = "_"
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like (ChrW(233) & "*"): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[?]": s = s & Ap(v)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "A1": b = " ": t = "z": u = "a1"
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ae": b = "B": t = "ss": u = "aBc"
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) And (t = u): s = s & Ap(v)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CDec("79228162514264337593543950335"): b = "B": t = "10": u = "x-y"
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "A?C": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1e3": b = "ss": t = "A": u = "?"
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[?]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ab": b = (ChrW(233)): t = "[": u = "E"
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "-0": b = True: t = "a": u = "1"
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[?]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = (ChrW(233)): b = "True": t = "10": u = "E"
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "aBc": b = "aBc": t = "_": u = "abc"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Eqv (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "A?C": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "#": b = "A": t = "9": u = "-1"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "9": b = "*": t = "*": u = (ChrW(198))
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "": b = " ": t = "Z": u = "1.5"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[a-]": s = s & Ap(v)
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "SS": b = "*": t = "a": u = "A"
    Err.Clear: v = Empty: v = (a = b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*c": s = s & Ap(v)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "z": b = (ChrW(233)): t = (ChrW(233)): u = "_"
    Err.Clear: v = Empty: v = t Like "?*?": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "a*": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ss": b = "-1": t = "ss": u = "-1"
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1": b = "_": t = "x-y": u = "-1"
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[a-]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "abd": b = "b": t = "?": u = "*"
    Err.Clear: v = Empty: v = a Like "[a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*[0-9]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "A1": b = "e": t = "ss": u = ""
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1": b = "B": t = (ChrW(223)): u = "z"
    Err.Clear: v = Empty: v = a Like "[*]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Xor (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t < u): s = s & Ap(v)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ABC": b = "z": t = "aBc": u = "e"
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "B": b = "x-y": t = "b": u = "A"
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "ss": s = s & Ap(v)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = Empty: b = "10": t = "A": u = "z"
    Err.Clear: v = Empty: v = (a > b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) And (t < u): s = s & Ap(v)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ab": b = "$5": t = "9": u = "b"
    Err.Clear: v = Empty: v = a Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*b*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "x-y": b = "A1": t = "10": u = "z"
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "A?C": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ss": b = "1.5": t = "ss": u = (ChrW(198))
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[A-Z]": s = s & Ap(v)
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = (ChrW(223)): b = Null: t = "ae": u = "A"
    Err.Clear: v = Empty: v = a Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?*?": s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "ss": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*b*": s = s & Ap(v)
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "false": b = "2.5": t = "1": u = "ae"
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "_": b = "ABC": t = "a1": u = (ChrW(223))
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like (ChrW(233) & "*"): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "e": b = "xy": t = "": u = "ab"
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "a[b]c": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "z": b = "10": t = "B": u = "#"
    Err.Clear: v = Empty: v = a Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "a*": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*c": s = s & Ap(v)
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "B": b = "1": t = "B": u = "1"
    Err.Clear: v = Empty: v = t Like "ss": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "ab": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) And (t = u): s = s & Ap(v)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1": b = Null: t = "#": u = "?"
    Err.Clear: v = Empty: v = (a > b) Xor (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Xor (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "a1": b = "?": t = "ae": u = (ChrW(198))
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = " ": b = "ab": t = " ": u = "2"
    Err.Clear: v = Empty: v = a Like "[!a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "ab": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Or (t = u): s = s & Ap(v)
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CDec("0.0000000000000000000000000001"): b = (ChrW(223)): t = "*": u = "z"
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like (ChrW(233) & "*"): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "-1": b = "2": t = "B": u = "ab"
    Err.Clear: v = Empty: v = a Like "[A-Z]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "a*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "A1": b = CSng((-7.211)): t = "2": u = "_"
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[?]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[a-c]*": s = s & Ap(v)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "$5": b = "x-y": t = " ": u = "a1"
    Err.Clear: v = Empty: v = a Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "Z": b = (ChrW(233)): t = "1": u = "1"
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "": s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "xy": b = "1": t = (ChrW(223)): u = (ChrW(233))
    Err.Clear: v = Empty: v = a Like "?": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "True": b = CInt(1000): t = "b": u = "-1"
    Err.Clear: v = Empty: v = (a > b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "A1": b = "  7  ": t = "xy": u = (ChrW(223))
    Err.Clear: v = Empty: v = a Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[!0-9]?": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Imp (t < u): s = s & Ap(v)
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "x-y": b = "aBc": t = "e": u = " "
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "e": b = "SS": t = "?": u = (ChrW(233))
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Eqv (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1E+400": b = #12/30/1899#: t = "z": u = "1.5"
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*[0-9]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[!0-9]?": s = s & Ap(v)
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ss": b = "ab": t = "x-y": u = "10"
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "A?C": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = True: b = "x-y": t = "b": u = "["
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Xor (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Or (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*b*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "": s = s & Ap(v)
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ss": b = "A1": t = "2": u = "["
    Err.Clear: v = Empty: v = a Like "[!a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Or (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "-0": b = "1": t = "A": u = "ss"
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "a*": s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "a*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ae": b = "": t = "SS": u = "a"
    Err.Clear: v = Empty: v = t Like "": s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "&H10": b = "True": t = "1": u = "?"
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*[0-9]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CSng((-333.65)): b = "1,234": t = "aBc": u = (ChrW(198))
    Err.Clear: v = Empty: v = a Like "ab": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "[": b = "10": t = "x-y": u = "abc"
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "E": b = "[": t = "2": u = "10"
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?*?": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[!a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "abc": b = "ss": t = "SS": u = "a"
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Or (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[a-c]*": s = s & Ap(v)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "SS": b = (ChrW(223)): t = "ABC": u = "True"
    Err.Clear: v = Empty: v = a Like "[*]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "##": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Or (t < u): s = s & Ap(v)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "b": b = Empty: t = "9": u = (ChrW(223))
    Err.Clear: v = Empty: v = (a = b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "xy": b = "e": t = "#": u = "9"
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Eqv (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "ab": s = s & Ap(v)
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = " 12 ": b = "E": t = "?": u = "B"
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[!a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[!0-9]?": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CInt(-2): b = "a": t = "True": u = "A"
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like (ChrW(233) & "*"): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = " 12 ": b = "ss": t = "_": u = "x-y"
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "a": b = "  7  ": t = (ChrW(233)): u = "b"
    Err.Clear: v = Empty: v = a Like "[a-]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ab": b = " ": t = "abc": u = "ae"
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[*]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "-1": b = "x-y": t = "a": u = "A"
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "-1": b = "x-y": t = "ss": u = "e"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "A?C": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1.5": b = "abc": t = " ": u = "1"
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*[0-9]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "AB": s = s & Ap(v)
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "?": b = "a1": t = "A1": u = "["
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "ab": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "&H10": b = " ": t = "ae": u = "B"
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*[0-9]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[!0-9]?": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "E": b = (ChrW(233)): t = (ChrW(198)): u = "A"
    Err.Clear: v = Empty: v = t Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Or (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = False: b = "*": t = "A1": u = "ab"
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like (ChrW(233) & "*"): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) And (t = u): s = s & Ap(v)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ABC": b = "z": t = "_": u = "xy"
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like (ChrW(233) & "*"): s = s & Ap(v)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "z": b = "ss": t = "1": u = "ae"
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "B": b = " ": t = "": u = "Z"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ae": b = "b": t = (ChrW(198)): u = "abc"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[!a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t < u): s = s & Ap(v)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "?": b = CLng(46340): t = "-1": u = "1"
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "a": b = CDec("1.5"): t = "x-y": u = "ss"
    Err.Clear: v = Empty: v = t Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "[": b = "[": t = "abd": u = "ab"
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = (ChrW(198)): b = CCur((-0.1)): t = "": u = (ChrW(223))
    Err.Clear: v = Empty: v = a Like "[*]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "a*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CInt(182): b = "-1": t = "E": u = "xy"
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Eqv (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "a*": s = s & Ap(v)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "True": b = "1": t = "[": u = "["
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[!a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "#": s = s & Ap(v)
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "E": b = "-1": t = "True": u = "A"
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?": s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "2": b = "a1": t = "ABC": u = "x-y"
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Or (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[!a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CSng((-91)): b = "abc": t = "a": u = (ChrW(223))
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "##": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[a-]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "10": b = "x-y": t = "z": u = "A1"
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[a-c]*": s = s & Ap(v)
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CInt(256): b = (ChrW(233)): t = "": u = "SS"
    Err.Clear: v = Empty: v = a Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[!0-9]?": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "abc": b = "1d2": t = (ChrW(198)): u = "a"
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ab": b = "[": t = "[": u = "ss"
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*b*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like (ChrW(233) & "*"): s = s & Ap(v)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "A": b = "2": t = "aBc": u = ""
    Err.Clear: v = Empty: v = a Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ABC": b = "z": t = "2": u = "Z"
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "ab": s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[?]": s = s & Ap(v)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ABC": b = CLng(2): t = "9": u = "a"
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Xor (t = u): s = s & Ap(v)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1/2/2000": b = "e": t = "abd": u = ""
    Err.Clear: v = Empty: v = t Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "9": b = "10": t = "a": u = " "
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "ss": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "SS": b = "#": t = "True": u = "b"
    Err.Clear: v = Empty: v = (a < b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[A-Z]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1e3": b = Empty: t = "a": u = "ABC"
    Err.Clear: v = Empty: v = a Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "9": b = "ae": t = (ChrW(223)): u = ""
    Err.Clear: v = Empty: v = (a = b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = True: b = "x-y": t = "#": u = ""
    Err.Clear: v = Empty: v = (a < b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ae": b = "-1": t = "ab": u = (ChrW(233))
    Err.Clear: v = Empty: v = a Like "[a-]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) And (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "AB": s = s & Ap(v)
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "abd": b = "-1": t = "z": u = "True"
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*b*": s = s & Ap(v)
    Err.Clear: v = Empty: v = t <> u: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Eqv (t = u): s = s & Ap(v)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "10": b = "5%": t = "SS": u = "z"
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[?]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "##": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "2": b = "abc": t = "Z": u = "B"
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CInt(32766): b = "_": t = "Z": u = "ABC"
    Err.Clear: v = Empty: v = t Like "[aeiou]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = t <= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Or (t < u): s = s & Ap(v)
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = (ChrW(233)): b = CSng((-406112.495)): t = "xy": u = "10"
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[a-]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "?*?": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like (ChrW(233) & "*"): s = s & Ap(v)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "*": b = "z": t = "1": u = ""
    Err.Clear: v = Empty: v = (a = b) And (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "-1": b = "A": t = "abc": u = "abd"
    Err.Clear: v = Empty: v = t Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Eqv (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "Z": b = "9": t = "xy": u = "A1"
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "##": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[*]": s = s & Ap(v)
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CDec("12345678901234567890.123"): b = "1.5": t = "x-y": u = "aBc"
    Err.Clear: v = Empty: v = t Like "[!0-9]?": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "A": b = "0x10": t = "_": u = "?"
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "ABC": b = "?": t = "z": u = "?"
    Err.Clear: v = Empty: v = (a > b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[a-]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "##": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "A": b = "a1": t = "A": u = "10"
    Err.Clear: v = Empty: v = t Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[A-Z]": s = s & Ap(v)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "1": b = "3.0": t = "xy": u = "x-y"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "#": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?": s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "3.0": b = "1": t = "?": u = "ABC"
    Err.Clear: v = Empty: v = a Like "a[b]c": s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = (ChrW(198)): b = CSng((-1E+300)): t = " ": u = "True"
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "##": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Imp (t = u): s = s & Ap(v)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "E": b = "": t = "Z": u = "z"
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*[0-9]": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Eqv (t = u): s = s & Ap(v)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "5%": b = True: t = "B": u = "1.5"
    Err.Clear: v = Empty: v = (a > b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Xor (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "A?C": s = s & Ap(v)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "-1": b = "x-y": t = "_": u = "ss"
    Err.Clear: v = Empty: v = (a = b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Xor (t = u): s = s & Ap(v)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "#": b = "2.5": t = "[": u = "A1"
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "10": b = "B": t = "A": u = "abc"
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "abc": b = "?": t = "e": u = "2"
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "?": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like (ChrW(233) & "*"): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Imp (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = CInt(7): b = "abd": t = "b": u = "1.5"
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t = u): s = s & Ap(v)
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "a1": b = "10": t = "A1": u = "z"
    Err.Clear: v = Empty: v = (a > b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "abc": b = "1": t = "Z": u = "_"
    Err.Clear: v = Empty: v = (a > b) Or (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a = b) Xor (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[a-c]*": s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?*?": s = s & Ap(v)
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = 0.00005@: b = "": t = "a1": u = "aBc"
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "AB": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Or (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "x-y": b = DateSerial(1957, 3, 1): t = "xy": u = "e"
    Err.Clear: v = Empty: v = t Like "": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "##": s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*c": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*c": s = s & Ap(v)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "9": b = "ABC": t = "[": u = "A"
    Err.Clear: v = Empty: v = a Like "[?]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[A-Z]": s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "10": b = "-3": t = "Z": u = (ChrW(198))
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a > b) Imp (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = t < u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "_": b = "ss": t = "1": u = (ChrW(233))
    Err.Clear: v = Empty: v = t > u: s = s & Ap(v)
    Err.Clear: v = Empty: v = t >= u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Imp b: s = s & Ap(v)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "e": b = "abc": t = "-1": u = "9"
    Err.Clear: v = Empty: v = a Like "a[b]c": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "A?C": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Or (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "aBc": b = "e": t = "a": u = "ae"
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "*": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b): s = s & Ap(v)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "abc": b = (ChrW(198)): t = "10": u = ""
    Err.Clear: v = Empty: v = (a < b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "[#]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?*?": s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "[a-]": s = s & Ap(v)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "2.5": b = "xy": t = "e": u = (ChrW(223))
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Xor (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) Eqv (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a < b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = (ChrW(233)): b = "e": t = "A1": u = "*"
    Err.Clear: v = Empty: v = a Like "[?]": s = s & Ap(v)
    Err.Clear: v = Empty: v = a Like "##": s = s & Ap(v)
    Err.Clear: v = Empty: v = t = u: s = s & Ap(v)
    Err.Clear: v = Empty: v = a >= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a > b: s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(t, u, -1): s = s & Ap(v)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "b": b = (ChrW(223)): t = "#": u = "SS"
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t < u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrComp(a, b, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <= b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "*[0-9]": s = s & Ap(v)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim a As Variant, b As Variant, t As String, u As String
    a = "b": b = " ": t = (ChrW(233)): u = "1"
    Err.Clear: v = Empty: v = a Like "ab": s = s & Ap(v)
    Err.Clear: v = Empty: v = t Like "?*?": s = s & Ap(v)
    Err.Clear: v = Empty: v = (a < b) And (t = u): s = s & Ap(v)
    Err.Clear: v = Empty: v = a <> b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a = b: s = s & Ap(v)
    Err.Clear: v = Empty: v = a Eqv b: s = s & Ap(v)
    C149 = s
End Function
