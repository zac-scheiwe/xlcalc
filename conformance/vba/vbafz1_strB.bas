Attribute VB_Name = "ModVbFz1StrB"
Option Compare Binary
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
Public Function VbFz1StrB(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1StrB = C0()
    Case 1: VbFz1StrB = C1()
    Case 2: VbFz1StrB = C2()
    Case 3: VbFz1StrB = C3()
    Case 4: VbFz1StrB = C4()
    Case 5: VbFz1StrB = C5()
    Case 6: VbFz1StrB = C6()
    Case 7: VbFz1StrB = C7()
    Case 8: VbFz1StrB = C8()
    Case 9: VbFz1StrB = C9()
    Case 10: VbFz1StrB = C10()
    Case 11: VbFz1StrB = C11()
    Case 12: VbFz1StrB = C12()
    Case 13: VbFz1StrB = C13()
    Case 14: VbFz1StrB = C14()
    Case 15: VbFz1StrB = C15()
    Case 16: VbFz1StrB = C16()
    Case 17: VbFz1StrB = C17()
    Case 18: VbFz1StrB = C18()
    Case 19: VbFz1StrB = C19()
    Case 20: VbFz1StrB = C20()
    Case 21: VbFz1StrB = C21()
    Case 22: VbFz1StrB = C22()
    Case 23: VbFz1StrB = C23()
    Case 24: VbFz1StrB = C24()
    Case 25: VbFz1StrB = C25()
    Case 26: VbFz1StrB = C26()
    Case 27: VbFz1StrB = C27()
    Case 28: VbFz1StrB = C28()
    Case 29: VbFz1StrB = C29()
    Case 30: VbFz1StrB = C30()
    Case 31: VbFz1StrB = C31()
    Case 32: VbFz1StrB = C32()
    Case 33: VbFz1StrB = C33()
    Case 34: VbFz1StrB = C34()
    Case 35: VbFz1StrB = C35()
    Case 36: VbFz1StrB = C36()
    Case 37: VbFz1StrB = C37()
    Case 38: VbFz1StrB = C38()
    Case 39: VbFz1StrB = C39()
    Case 40: VbFz1StrB = C40()
    Case 41: VbFz1StrB = C41()
    Case 42: VbFz1StrB = C42()
    Case 43: VbFz1StrB = C43()
    Case 44: VbFz1StrB = C44()
    Case 45: VbFz1StrB = C45()
    Case 46: VbFz1StrB = C46()
    Case 47: VbFz1StrB = C47()
    Case 48: VbFz1StrB = C48()
    Case 49: VbFz1StrB = C49()
    Case 50: VbFz1StrB = C50()
    Case 51: VbFz1StrB = C51()
    Case 52: VbFz1StrB = C52()
    Case 53: VbFz1StrB = C53()
    Case 54: VbFz1StrB = C54()
    Case 55: VbFz1StrB = C55()
    Case 56: VbFz1StrB = C56()
    Case 57: VbFz1StrB = C57()
    Case 58: VbFz1StrB = C58()
    Case 59: VbFz1StrB = C59()
    Case 60: VbFz1StrB = C60()
    Case 61: VbFz1StrB = C61()
    Case 62: VbFz1StrB = C62()
    Case 63: VbFz1StrB = C63()
    Case 64: VbFz1StrB = C64()
    Case 65: VbFz1StrB = C65()
    Case 66: VbFz1StrB = C66()
    Case 67: VbFz1StrB = C67()
    Case 68: VbFz1StrB = C68()
    Case 69: VbFz1StrB = C69()
    Case 70: VbFz1StrB = C70()
    Case 71: VbFz1StrB = C71()
    Case 72: VbFz1StrB = C72()
    Case 73: VbFz1StrB = C73()
    Case 74: VbFz1StrB = C74()
    Case 75: VbFz1StrB = C75()
    Case 76: VbFz1StrB = C76()
    Case 77: VbFz1StrB = C77()
    Case 78: VbFz1StrB = C78()
    Case 79: VbFz1StrB = C79()
    Case 80: VbFz1StrB = C80()
    Case 81: VbFz1StrB = C81()
    Case 82: VbFz1StrB = C82()
    Case 83: VbFz1StrB = C83()
    Case 84: VbFz1StrB = C84()
    Case 85: VbFz1StrB = C85()
    Case 86: VbFz1StrB = C86()
    Case 87: VbFz1StrB = C87()
    Case 88: VbFz1StrB = C88()
    Case 89: VbFz1StrB = C89()
    Case 90: VbFz1StrB = C90()
    Case 91: VbFz1StrB = C91()
    Case 92: VbFz1StrB = C92()
    Case 93: VbFz1StrB = C93()
    Case 94: VbFz1StrB = C94()
    Case 95: VbFz1StrB = C95()
    Case 96: VbFz1StrB = C96()
    Case 97: VbFz1StrB = C97()
    Case 98: VbFz1StrB = C98()
    Case 99: VbFz1StrB = C99()
    Case 100: VbFz1StrB = C100()
    Case 101: VbFz1StrB = C101()
    Case 102: VbFz1StrB = C102()
    Case 103: VbFz1StrB = C103()
    Case 104: VbFz1StrB = C104()
    Case 105: VbFz1StrB = C105()
    Case 106: VbFz1StrB = C106()
    Case 107: VbFz1StrB = C107()
    Case 108: VbFz1StrB = C108()
    Case 109: VbFz1StrB = C109()
    Case 110: VbFz1StrB = C110()
    Case 111: VbFz1StrB = C111()
    Case 112: VbFz1StrB = C112()
    Case 113: VbFz1StrB = C113()
    Case 114: VbFz1StrB = C114()
    Case 115: VbFz1StrB = C115()
    Case 116: VbFz1StrB = C116()
    Case 117: VbFz1StrB = C117()
    Case 118: VbFz1StrB = C118()
    Case 119: VbFz1StrB = C119()
    Case 120: VbFz1StrB = C120()
    Case 121: VbFz1StrB = C121()
    Case 122: VbFz1StrB = C122()
    Case 123: VbFz1StrB = C123()
    Case 124: VbFz1StrB = C124()
    Case 125: VbFz1StrB = C125()
    Case 126: VbFz1StrB = C126()
    Case 127: VbFz1StrB = C127()
    Case 128: VbFz1StrB = C128()
    Case 129: VbFz1StrB = C129()
    Case 130: VbFz1StrB = C130()
    Case 131: VbFz1StrB = C131()
    Case 132: VbFz1StrB = C132()
    Case 133: VbFz1StrB = C133()
    Case 134: VbFz1StrB = C134()
    Case 135: VbFz1StrB = C135()
    Case 136: VbFz1StrB = C136()
    Case 137: VbFz1StrB = C137()
    Case 138: VbFz1StrB = C138()
    Case 139: VbFz1StrB = C139()
    Case 140: VbFz1StrB = C140()
    Case 141: VbFz1StrB = C141()
    Case 142: VbFz1StrB = C142()
    Case 143: VbFz1StrB = C143()
    Case 144: VbFz1StrB = C144()
    Case 145: VbFz1StrB = C145()
    Case 146: VbFz1StrB = C146()
    Case 147: VbFz1StrB = C147()
    Case 148: VbFz1StrB = C148()
    Case 149: VbFz1StrB = C149()
    Case 150: VbFz1StrB = C150()
    Case 151: VbFz1StrB = C151()
    Case 152: VbFz1StrB = C152()
    Case 153: VbFz1StrB = C153()
    Case 154: VbFz1StrB = C154()
    Case 155: VbFz1StrB = C155()
    Case 156: VbFz1StrB = C156()
    Case 157: VbFz1StrB = C157()
    Case 158: VbFz1StrB = C158()
    Case 159: VbFz1StrB = C159()
    Case 160: VbFz1StrB = C160()
    Case 161: VbFz1StrB = C161()
    Case 162: VbFz1StrB = C162()
    Case 163: VbFz1StrB = C163()
    Case 164: VbFz1StrB = C164()
    Case 165: VbFz1StrB = C165()
    Case 166: VbFz1StrB = C166()
    Case 167: VbFz1StrB = C167()
    Case 168: VbFz1StrB = C168()
    Case 169: VbFz1StrB = C169()
    End Select
    Exit Function
EH:
    VbFz1StrB = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (" cc" & ChrW(223) & "B-aBc"): u = ""
    Err.Clear: v = Empty: v = StrConv(t, 128): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, (ChrW(233) & "Ab ")): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    Err.Clear: Mid(t, 100) = u: s = s & Ap(t)
    Err.Clear: Mid(t, -1) = u: s = s & Ap(t)
    s = s & Ap(t)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "AaBb1A": u = ("aB" & ChrW(233) & "c")
    Err.Clear: f8 = ("Ab" & ChrW(233) & "BAA-"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "B": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Right(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 5): s = s & Ap(v)
    Err.Clear: f8 = ("B;xa,aAb" & ChrW(233) & ChrW(233) & ";"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("aBAbb1 c" & ChrW(233)): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Len(t): s = s & Ap(v)
    Err.Clear: Mid(t, 2.5) = "abAbaa": s = s & Ap(t)
    s = s & Ap(t)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("cb" & ChrW(201)): u = (ChrW(201) & ";" & ChrW(201))
    Err.Clear: f8 = ("ab" & ChrW(223)): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("X," & ChrW(201) & "c 1"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(201) & "AbAb"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ";ca": s = s & Ap(f8)
    Err.Clear: f8 = "ab": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("," & ChrW(201) & ChrW(233) & " Ab,"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "-", False): s = s & Ap(v)
    s = s & Ap(t)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (";1 Ab" & ChrW(223) & "ab-"): u = "b,aB"
    Err.Clear: f8 = (ChrW(201) & ChrW(201) & "Abac"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "aab,Ab,aB": s = s & Ap(f8)
    Err.Clear: v = Empty: v = LenB(t): s = s & Ap(v)
    Err.Clear: Mid(t, -1) = "BaB": s = s & Ap(t)
    Err.Clear: v = Empty: v = Len(t): s = s & Ap(v)
    Err.Clear: f8 = (";BXaba" & ChrW(223)): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "": s = s & Ap(f8)
    Err.Clear: Mid$(t, 2) = u: s = s & Ap(t)
    s = s & Ap(t)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (", ab1aBAbc" & ChrW(201) & ";a" & ChrW(233)): u = "1x"
    Err.Clear: v = Empty: v = Mid(t, 0, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(-1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "ab"): s = s & Ap(v)
    Err.Clear: LSet t = (" " & ChrW(223) & ChrW(223) & "X;" & ChrW(223) & ChrW(223) & "1;BXaaB;"): s = s & Ap(t)
    Err.Clear: Mid(t, 3, 100) = "Ab;1": s = s & Ap(t)
    s = s & Ap(t)
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("Ab;,AbaBc" & ChrW(233) & "x"): u = ",baB"
    Err.Clear: v = Empty: v = Filter(Split(t, " "), (ChrW(233) & ";")): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "b", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(5, "ab"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 2): s = s & Ap(v)
    s = s & Ap(t)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(201) & ChrW(233) & "ab;aBaBAAb" & ChrW(223)): u = "a-"
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(0, 65): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "B"): s = s & Ap(v)
    s = s & Ap(t)
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("-aAb" & ChrW(233) & "x" & ChrW(233) & ChrW(201) & "1AxaB"): u = "-Ab1"
    Err.Clear: v = Empty: v = Left$(t, 1.5): s = s & Ap(v)
    Err.Clear: f8 = "aB": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("aaB-Ab" & ChrW(233) & "Ab ;X"): s = s & Ap(f8)
    Err.Clear: RSet t = "abc": s = s & Ap(t)
    Err.Clear: Mid(t, 2, -1) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Right$(t, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "x", True, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("a1" & ChrW(223)): u = "abxa"
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "aB", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "aB"): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 128): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    s = s & Ap(t)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("abaBB,ccaaB" & ChrW(201) & "-,"): u = (ChrW(201))
    Err.Clear: LSet t = "": s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, u, 3): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, "a", 2, vbTextCompare): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: Mid(t, 100) = "bXc": s = s & Ap(t)
    Err.Clear: Mid(t, 100) = ",": s = s & Ap(t)
    s = s & Ap(t)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "X": u = ("1" & ChrW(233) & "-")
    Err.Clear: LSet t = (ChrW(201) & ",c" & ChrW(233) & "Aba" & ChrW(201) & "aBxA" & ChrW(223) & "aB"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    Err.Clear: Mid(t, 1, 0) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(3, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 0): s = s & Ap(v)
    s = s & Ap(t)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aaBAbx-" & ChrW(233)): u = ""
    Err.Clear: v = Empty: v = Replace(t, u, "ab ", 2, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(100, 321): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 1): s = s & Ap(v)
    Err.Clear: LSet t = (ChrW(223) & "c" & ChrW(201) & "cc-" & ChrW(201) & "axab;A-"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, 0, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), (ChrW(201) & "X"), False): s = s & Ap(v)
    s = s & Ap(t)
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(201) & "bbbBaAb"): u = (ChrW(201))
    Err.Clear: v = Empty: v = Left$(t, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 2): s = s & Ap(v)
    Err.Clear: LSet t = ("1Ab,aB" & ChrW(201) & " " & ChrW(223) & "a,"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 1): s = s & Ap(v)
    Err.Clear: Mid(t, -1) = u: s = s & Ap(t)
    Err.Clear: Mid(t, 0) = u: s = s & Ap(t)
    s = s & Ap(t)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & ";-aB"): u = "x"
    Err.Clear: v = Empty: v = Mid(t, 10, 1.5): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(223) & "Bc"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("A1aB" & ChrW(223) & " a"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = String(3, ""): s = s & Ap(v)
    Err.Clear: f8 = ("aB," & ChrW(201) & "1"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "Ab": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 5, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    s = s & Ap(t)
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("A;" & ChrW(201) & "baB1-xX"): u = "a"
    Err.Clear: RSet t = "A": s = s & Ap(t)
    Err.Clear: f8 = "A": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "Ab": s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStrRev(t, u, 100, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "A"): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", False): s = s & Ap(v)
    s = s & Ap(t)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "1abA c ab,BaB": u = ""
    Err.Clear: a = Empty: a = Split(t, " "): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 0): s = s & Ap(v)
    Err.Clear: LSet t = ("c;;" & ChrW(223) & "aBA,aB" & ChrW(201) & ",,-"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Right(t, 5): s = s & Ap(v)
    Err.Clear: Mid(t, -1, 1.5) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Right$(t, 2): s = s & Ap(v)
    s = s & Ap(t)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & ";aBcxAb-"): u = "--A"
    Err.Clear: v = Empty: v = Replace(t, u, "b-", 0, -1, 1): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(233)): s = s & Ap(t)
    Err.Clear: f8 = (ChrW(223) & "b1b b"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "1Bc": s = s & Ap(f8)
    Err.Clear: Mid(t, 2) = u: s = s & Ap(t)
    Err.Clear: f8 = "Ab-aB  cxx": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "XX1": s = s & Ap(f8)
    Err.Clear: Mid$(t, 10) = u: s = s & Ap(t)
    s = s & Ap(t)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("A-X" & ChrW(201) & "ababab"): u = ""
    Err.Clear: v = Empty: v = Replace(t, u, (ChrW(223) & " aB"), 1, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "c", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(-1, t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), ";Ab"): s = s & Ap(v)
    s = s & Ap(t)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "abaBx-Ax": u = ""
    Err.Clear: v = Empty: v = Mid(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), ";"): s = s & Ap(v)
    Err.Clear: Mid(t, 3, 2) = " ": s = s & Ap(t)
    Err.Clear: v = Empty: v = Len(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Len(t): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, "a", 2, vbTextCompare): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    s = s & Ap(t)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("b1aB," & ChrW(223) & "abcb"): u = (ChrW(201) & "Xb")
    Err.Clear: v = Empty: v = Filter(Split(t, ","), ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 1): s = s & Ap(v)
    Err.Clear: f8 = "ab1": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "b;abxB": s = s & Ap(f8)
    Err.Clear: Mid(t, 3) = "xa": s = s & Ap(t)
    Err.Clear: v = Empty: v = Replace(t, u, "", -1): s = s & Ap(v)
    s = s & Ap(t)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "Abb,ABaB": u = ","
    Err.Clear: v = Empty: v = InStr(1.5, t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(5, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), ""): s = s & Ap(v)
    Err.Clear: f8 = ("ab" & ChrW(201) & "A" & ChrW(233) & "c" & ChrW(233)): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("1abAb," & ChrW(201) & "-" & ChrW(223) & "1"): s = s & Ap(f8)
    Err.Clear: f8 = ("ab" & ChrW(201) & "Bb-"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("1" & ChrW(233)): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Right$(t, 2): s = s & Ap(v)
    s = s & Ap(t)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "-ax,Xxxb": u = "A"
    Err.Clear: v = Empty: v = Mid(t, 3, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, " "): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, " "): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbUpperCase): s = s & Ap(v)
    Err.Clear: LSet t = (ChrW(233) & "XAbxAbab"): s = s & Ap(t)
    s = s & Ap(t)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "AbBbx": u = ""
    Err.Clear: v = Empty: v = Right$(t, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrReverse(t): s = s & Ap(v)
    Err.Clear: Mid(t, 0, 1.5) = ("1B" & ChrW(223)): s = s & Ap(t)
    Err.Clear: LSet t = "": s = s & Ap(t)
    Err.Clear: Mid(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, vbUpperCase): s = s & Ap(v)
    s = s & Ap(t)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("b;,-," & ChrW(201) & ",Bb"): u = "ab1"
    Err.Clear: v = Empty: v = Right(t, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 5): s = s & Ap(v)
    Err.Clear: f8 = "ab": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Filter(Split(t, u), ("B" & ChrW(223))): s = s & Ap(v)
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    Err.Clear: Mid(t, 0, 100) = ("b-c" & ChrW(233)): s = s & Ap(t)
    s = s & Ap(t)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "X Xa": u = "c"
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "-"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(100): s = s & Ap(v)
    Err.Clear: Mid$(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    Err.Clear: f8 = ("ab" & ChrW(223) & "1 B" & ChrW(223) & ChrW(223) & "A1"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "XAb 1 ": s = s & Ap(f8)
    s = s & Ap(t)
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(201)): u = "c"
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbUpperCase): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(10, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u): s = s & Ap(v)
    s = s & Ap(t)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("Ab" & ChrW(223) & ChrW(233) & " "): u = "Ab"
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 0): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 3, 100): s = s & Ap(v)
    Err.Clear: Mid(t, -1) = ("c" & ChrW(223) & "ab"): s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 2, -1, 1): s = s & Ap(v)
    s = s & Ap(t)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (",;" & ChrW(223) & " ,"): u = ","
    Err.Clear: v = Empty: v = Mid(t, 10, 0): s = s & Ap(v)
    Err.Clear: Mid(t, 3, 100) = "BB": s = s & Ap(t)
    Err.Clear: LSet t = ("aBX1" & ChrW(233) & "x" & ChrW(233) & ChrW(223) & ChrW(233) & ";aB" & ChrW(223)): s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(-1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    s = s & Ap(t)
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "ab": u = "ca-"
    Err.Clear: v = Empty: v = Mid(t, 100, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(100, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: LSet t = "bbc1,,x;X cBab": s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: LSet t = ("aabacaBaBA;" & ChrW(223) & "ab1, B"): s = s & Ap(t)
    s = s & Ap(t)
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = (ChrW(201) & "X" & ChrW(201))
    Err.Clear: Mid(t, 2.5) = u: s = s & Ap(t)
    Err.Clear: LSet t = "XXAbxAbXbBB": s = s & Ap(t)
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    Err.Clear: Mid(t, 2.5) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Left(t, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 2.5): s = s & Ap(v)
    s = s & Ap(t)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("-Abc;abB" & ChrW(233) & "b"): u = (ChrW(233))
    Err.Clear: f8 = "x ,ab": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "Xbb": s = s & Ap(f8)
    Err.Clear: a = Empty: a = Split(t, u, 1.5): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "1ab", False): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(201) & "Aa1" & ChrW(223) & "Ab--"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "ab": s = s & Ap(f8)
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    s = s & Ap(t)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = " A": u = "abB"
    Err.Clear: LSet t = "AbX aA ": s = s & Ap(t)
    Err.Clear: v = Empty: v = Filter(Split(t, u), (ChrW(233) & ";"), True, vbTextCompare): s = s & Ap(v)
    Err.Clear: Mid(t, 10) = (ChrW(233) & "ab"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Replace(t, u, (ChrW(201)), 3, -1, 1): s = s & Ap(v)
    Err.Clear: LSet t = (ChrW(201) & ChrW(233)): s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    s = s & Ap(t)
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("X1" & ChrW(233) & "-" & ChrW(201) & " BaBx"): u = ""
    Err.Clear: v = Empty: v = Right$(t, 10): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: Mid(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(100, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 128): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(2.5, t, u, 1): s = s & Ap(v)
    s = s & Ap(t)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (";" & ChrW(233) & " aBbBAb1"): u = "B"
    Err.Clear: v = Empty: v = InStr(1.5, t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: RSet t = ("bc,B" & ChrW(233)): s = s & Ap(t)
    Err.Clear: v = Empty: v = Left$(t, 100): s = s & Ap(v)
    Err.Clear: Mid(t, 2.5) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(2, 65): s = s & Ap(v)
    s = s & Ap(t)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("a1" & ChrW(223) & ChrW(223) & "aAb"): u = ""
    Err.Clear: v = Empty: v = Mid(t, 1.5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(1.5, "ab"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 2.5): s = s & Ap(v)
    Err.Clear: Mid(t, 1) = "": s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, "a", 2, vbTextCompare): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    s = s & Ap(t)
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("," & ChrW(201) & "AbAb1AbaB"): u = ""
    Err.Clear: Mid(t, 0) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Trim(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: Mid(t, 100) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(10): s = s & Ap(v)
    s = s & Ap(t)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "ab B;;BabBaa": u = ("-aB" & ChrW(233))
    Err.Clear: v = Empty: v = String(0, "ab"): s = s & Ap(v)
    Err.Clear: f8 = ("c, 1-c" & ChrW(201)): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("aB" & ChrW(233) & "XAxaaBa"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "Bx"): s = s & Ap(v)
    s = s & Ap(t)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "A": u = "XAab"
    Err.Clear: v = Empty: v = String(3, 65): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), ("Ab" & ChrW(223))): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "b", True, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ";bx": u = ""
    Err.Clear: LSet t = "1,": s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(2, 65): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 5): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 1.5, 1): s = s & Ap(v)
    s = s & Ap(t)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aa,,A": u = ("ab1" & ChrW(223))
    Err.Clear: v = Empty: v = Replace(t, u, "ab", 1, 3): s = s & Ap(v)
    Err.Clear: f8 = ("1" & ChrW(223) & "b;b,1Ab"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "-c;": s = s & Ap(f8)
    Err.Clear: RSet t = ("c " & ChrW(223) & ",aab" & ChrW(233) & "c"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3): s = s & Ap(v)
    Err.Clear: Mid(t, 3, 1.5) = "": s = s & Ap(t)
    s = s & Ap(t)
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "ab,B": u = "aB; "
    Err.Clear: v = Empty: v = StrConv(t, 64): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3 + 64): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1.5): s = s & Ap(v)
    Err.Clear: Mid$(t, 1) = u: s = s & Ap(t)
    Err.Clear: f8 = ("Ab1; bab" & ChrW(201) & " b"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "b-": s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    s = s & Ap(t)
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (",A1A" & ChrW(201) & ";1BxA"): u = ""
    Err.Clear: v = Empty: v = Filter(Split(t, u), "AB", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "A", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: LSet t = "ab": s = s & Ap(t)
    Err.Clear: v = Empty: v = UCase(t): s = s & Ap(v)
    s = s & Ap(t)
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(223) & "Aba" & ChrW(223)): u = "XAbx"
    Err.Clear: v = Empty: v = Replace(t, u, "x", 1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3 + 64): s = s & Ap(v)
    Err.Clear: Mid(t, 2, -1) = ("Ab" & ChrW(233) & "Abab"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "AaB", 1, -1, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = "Abc"
    Err.Clear: v = Empty: v = Mid(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(3): s = s & Ap(v)
    Err.Clear: LSet t = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = String(10, 65): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 64): s = s & Ap(v)
    Err.Clear: Mid(t, 2) = "abaBaB": s = s & Ap(t)
    s = s & Ap(t)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "AbcB;;;Ax": u = "abaB1"
    Err.Clear: Mid$(t, 2.5) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Left(t, 1): s = s & Ap(v)
    Err.Clear: Mid$(t, -1) = u: s = s & Ap(t)
    Err.Clear: f8 = "XAAbAb": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "aB,cAb-": s = s & Ap(f8)
    Err.Clear: Mid(t, -1) = u: s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    s = s & Ap(t)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (";" & ChrW(201) & ChrW(233) & "XaBAb"): u = " -"
    Err.Clear: RSet t = ("BA,;" & ChrW(201) & ChrW(233) & "Abab" & ChrW(233) & "xX" & ChrW(223) & "a"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "aB", 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 0, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 1): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    s = s & Ap(t)
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = (ChrW(201) & "a ")
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(100): s = s & Ap(v)
    Err.Clear: Mid(t, 5) = "-aB1": s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 3 + 64): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 3, 2.5): s = s & Ap(v)
    s = s & Ap(t)
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ""
    Err.Clear: RSet t = (ChrW(201) & ChrW(223) & "-," & ChrW(233) & "ab"): s = s & Ap(t)
    Err.Clear: f8 = ("aB" & ChrW(223) & " -"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(233) & "Bc-X"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 2.5): s = s & Ap(v)
    Err.Clear: LSet t = "-1XabaB": s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    Err.Clear: f8 = "bb1cAbAca": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (";cXc" & ChrW(233) & ChrW(223) & ";1X"): s = s & Ap(f8)
    s = s & Ap(t)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & ";,caaA aXa"): u = "aB"
    Err.Clear: v = Empty: v = Space(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "1ab", -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(3): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(100, 321): s = s & Ap(v)
    s = s & Ap(t)
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("abbXaBXA" & ChrW(233) & " b"): u = "BB"
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "BaB"): s = s & Ap(v)
    Err.Clear: v = Empty: v = LTrim(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "Ba"): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(100, ""): s = s & Ap(v)
    s = s & Ap(t)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aBaB": u = "B;b"
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 10, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    s = s & Ap(t)
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("a1a" & ChrW(201) & "1-,ab b" & ChrW(233) & "B"): u = "A"
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: Mid(t, -1) = u: s = s & Ap(t)
    Err.Clear: RSet t = ("XaAbXcxAXAbaB" & ChrW(201) & "ab; "): s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 10, 3): s = s & Ap(v)
    Err.Clear: Mid(t, 1, 10) = "ab,b": s = s & Ap(t)
    Err.Clear: Mid(t, 2.5) = u: s = s & Ap(t)
    s = s & Ap(t)
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ",-aBccX": u = (";" & ChrW(201))
    Err.Clear: v = Empty: v = Asc(t): s = s & Ap(v)
    Err.Clear: Mid(t, 2) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    Err.Clear: Mid$(t, 3) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 3 + 64): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrReverse(t): s = s & Ap(v)
    s = s & Ap(t)
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "B1BaA": u = ""
    Err.Clear: a = Empty: a = Split(t, u, 100): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, (ChrW(233) & "x"), 5): s = s & Ap(v)
    Err.Clear: Mid(t, 5, 5) = "1Ab1B": s = s & Ap(t)
    s = s & Ap(t)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & ChrW(233) & "-bxab"): u = "cx"
    Err.Clear: v = Empty: v = StrConv(t, 128): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 100, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, ("XAb" & ChrW(233)), 1.5): s = s & Ap(v)
    s = s & Ap(t)
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(223)): u = "BXX"
    Err.Clear: v = Empty: v = Mid(t, 1.5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrReverse(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 64): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    s = s & Ap(t)
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("abX" & ChrW(201)): u = ""
    Err.Clear: v = Empty: v = Left(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 2): s = s & Ap(v)
    Err.Clear: f8 = ("xcAb," & ChrW(233) & "XBAbaB"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "": s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    s = s & Ap(t)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("1-Ab" & ChrW(223)): u = "x-"
    Err.Clear: LSet t = (ChrW(223) & "X11;aA- "): s = s & Ap(t)
    Err.Clear: v = Empty: v = String(2, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(1.5, t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 1, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, ("1" & ChrW(233)), 2.5): s = s & Ap(v)
    s = s & Ap(t)
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("b" & ChrW(201) & "AbcXXab;A" & ChrW(223)): u = ";x"
    Err.Clear: v = Empty: v = InStrRev(t, u, 1.5, 1): s = s & Ap(v)
    Err.Clear: Mid(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = StrReverse(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, ("c" & ChrW(223)), 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, "a", 2, vbTextCompare): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    s = s & Ap(t)
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = "c"
    Err.Clear: v = Empty: v = InStr(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(2.5, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 3, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = LCase(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", True, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "1;aB; aBbX;X": u = (ChrW(223))
    Err.Clear: RSet t = (" " & ChrW(223) & "-;" & ChrW(201) & "b1,;1a,-aB"): s = s & Ap(t)
    Err.Clear: Mid(t, 1.5) = "xX,": s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, (ChrW(233) & "c"), 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(1, t, u, 1): s = s & Ap(v)
    Err.Clear: Mid(t, 100) = "": s = s & Ap(t)
    s = s & Ap(t)
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (" " & ChrW(233) & ChrW(233)): u = (ChrW(223) & ChrW(201))
    Err.Clear: v = Empty: v = StrReverse(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "aB", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(1, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: Mid$(t, 10) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(-1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(1): s = s & Ap(v)
    s = s & Ap(t)
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "acaAba-": u = (ChrW(223) & "Ab")
    Err.Clear: v = Empty: v = InStrRev(t, u, 1.5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, "a", 2, vbTextCompare): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: f8 = "a b": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (";" & ChrW(223) & ChrW(223) & "-a" & ChrW(223) & ",;"): s = s & Ap(f8)
    Err.Clear: f8 = "AB": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "": s = s & Ap(f8)
    s = s & Ap(t)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ";,;x": u = "bX"
    Err.Clear: v = Empty: v = InStrRev(t, u, 100, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "AbAX"): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 2): s = s & Ap(v)
    s = s & Ap(t)
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "Ab1": u = "A11"
    Err.Clear: v = Empty: v = Mid(t, 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3 + 64): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), (ChrW(233)), True, vbTextCompare): s = s & Ap(v)
    Err.Clear: Mid$(t, -1) = u: s = s & Ap(t)
    s = s & Ap(t)
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("bb" & ChrW(223) & ChrW(233) & "Xaa,-b aB"): u = "1"
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "Ab", 1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 1): s = s & Ap(v)
    s = s & Ap(t)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ""
    Err.Clear: v = Empty: v = Right$(t, 100): s = s & Ap(v)
    Err.Clear: Mid(t, 5, 10) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Len(t): s = s & Ap(v)
    Err.Clear: Mid(t, 3, 2) = "": s = s & Ap(t)
    s = s & Ap(t)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "1": u = (ChrW(223))
    Err.Clear: v = Empty: v = InStr(3, t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 10, 1): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, " aBX"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "ab 1", 10, -1, 1): s = s & Ap(v)
    s = s & Ap(t)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "AcAbb- ": u = "x- "
    Err.Clear: f8 = (";aB" & ChrW(201) & ChrW(223) & "cB,Ba"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("AAbAbab;-ab" & ChrW(201) & " "): s = s & Ap(f8)
    Err.Clear: a = Empty: a = Split(t): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: Mid(t, 5, 1.5) = ("1" & ChrW(201) & "aB"): s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(-1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "x"): s = s & Ap(v)
    s = s & Ap(t)
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("XX" & ChrW(233) & ChrW(223) & "ab"): u = (ChrW(223))
    Err.Clear: v = Empty: v = String(10, 65): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 10, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = UCase(t): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, "a", 2, vbTextCompare): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: Mid(t, 1) = ("1" & ChrW(223) & "a,"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Right(t, 0): s = s & Ap(v)
    s = s & Ap(t)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ""
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "AbX "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 100, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(1, t, u, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("Ab" & ChrW(233) & "1," & ChrW(201) & "-ab"): u = "x"
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    Err.Clear: f8 = "X": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("ab" & ChrW(233) & "Ab" & ChrW(201)): s = s & Ap(f8)
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 10): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(201) & "AbB" & ChrW(201) & ChrW(233) & ChrW(223) & "ab"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("B" & ChrW(223) & "abc"): s = s & Ap(f8)
    s = s & Ap(t)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("a-B" & ChrW(233) & ChrW(223) & "; " & ChrW(201) & "B"): u = ""
    Err.Clear: v = Empty: v = InStrRev(t, u, 0, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "Bb"): s = s & Ap(v)
    Err.Clear: RSet t = (";b;x-" & ChrW(201) & "XBaB" & ChrW(233) & "bx"): s = s & Ap(t)
    Err.Clear: f8 = "": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("Bax" & ChrW(201)): s = s & Ap(f8)
    Err.Clear: Mid(t, 100) = "AbaB": s = s & Ap(t)
    Err.Clear: LSet t = "XaB;": s = s & Ap(t)
    s = s & Ap(t)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("abaB" & ChrW(223) & "xB"): u = ("x" & ChrW(201) & ChrW(223))
    Err.Clear: Mid(t, 2) = u: s = s & Ap(t)
    Err.Clear: Mid(t, 10) = "cXAb": s = s & Ap(t)
    Err.Clear: v = Empty: v = Trim(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 5, 1): s = s & Ap(v)
    s = s & Ap(t)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("b" & ChrW(201) & "b"): u = (ChrW(223) & ";")
    Err.Clear: v = Empty: v = Left(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "Bab"): s = s & Ap(v)
    Err.Clear: LSet t = (",xab" & ChrW(233) & ChrW(223) & ChrW(223) & ChrW(201)): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 0): s = s & Ap(v)
    s = s & Ap(t)
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aBAb,Ab,ab" & ChrW(201) & "xb" & ChrW(201) & "AaB"): u = "cx"
    Err.Clear: Mid$(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(2, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    Err.Clear: Mid(t, -1) = "bxB": s = s & Ap(t)
    s = s & Ap(t)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ""
    Err.Clear: v = Empty: v = String(3, "ab"): s = s & Ap(v)
    Err.Clear: RSet t = "": s = s & Ap(t)
    Err.Clear: LSet t = "BXAb1ab": s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(100): s = s & Ap(v)
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "A"): s = s & Ap(v)
    s = s & Ap(t)
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "B1 ": u = ""
    Err.Clear: v = Empty: v = InStr(1.5, t, u): s = s & Ap(v)
    Err.Clear: Mid(t, 10) = u: s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(223) & "x"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    s = s & Ap(t)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & "1"): u = "B"
    Err.Clear: v = Empty: v = Mid(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = LenB(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 2): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 3, -1): s = s & Ap(v)
    s = s & Ap(t)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("xc;;" & ChrW(223) & ";;-"): u = ("X" & ChrW(233) & "A")
    Err.Clear: Mid$(t, 100) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = String(1, u): s = s & Ap(v)
    Err.Clear: f8 = ", -ab;,,x aB": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "X Aab": s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStrRev(t, u, 5): s = s & Ap(v)
    Err.Clear: LSet t = "abab a": s = s & Ap(t)
    Err.Clear: v = Empty: v = Filter(Split(t, u), ";", True, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("cX" & ChrW(223) & "X-Abax"): u = ""
    Err.Clear: v = Empty: v = Replace(t, u, "", 10): s = s & Ap(v)
    Err.Clear: f8 = "AbacaAbAbb-X1": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ",XabXb;-": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 5, 3): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(223) & "1" & ChrW(201) & "aA"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(223)): s = s & Ap(f8)
    Err.Clear: v = Empty: v = LCase(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(100): s = s & Ap(v)
    s = s & Ap(t)
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("1" & ChrW(233)): u = ""
    Err.Clear: v = Empty: v = Space(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 0): s = s & Ap(v)
    Err.Clear: f8 = ("cxc1cx" & ChrW(233) & " ,A"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "": s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(10, "ab"): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    s = s & Ap(t)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "AbBA": u = "aB"
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, " "): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 3, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = LCase(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbUpperCase): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(223) & "aB"): u = "-"
    Err.Clear: v = Empty: v = Replace(t, u, "", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "A"): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(201) & "-Bab,;"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = LCase(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 1): s = s & Ap(v)
    s = s & Ap(t)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & "babAb "): u = ""
    Err.Clear: v = Empty: v = Left$(t, 5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(100, t, u, 1): s = s & Ap(v)
    Err.Clear: LSet t = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(2): s = s & Ap(v)
    Err.Clear: v = Empty: v = LCase(t): s = s & Ap(v)
    s = s & Ap(t)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("AAaBBB" & ChrW(223) & ChrW(201)): u = (ChrW(223))
    Err.Clear: v = Empty: v = StrConv(t, vbUpperCase): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: LSet t = (ChrW(201) & ",Ab-Xb" & ChrW(233) & ChrW(233) & "a-" & ChrW(233) & "bx "): s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), " ", True, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ""
    Err.Clear: Mid(t, 100) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Left(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 1.5): s = s & Ap(v)
    Err.Clear: f8 = "X1 ": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("A1abc,aB" & ChrW(201) & "x"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, -1, 2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 0, 1): s = s & Ap(v)
    s = s & Ap(t)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "1": u = ""
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = UCase(t): s = s & Ap(v)
    Err.Clear: f8 = ("-" & ChrW(201) & ";Xc;"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (",c,Ab-" & ChrW(233) & ChrW(201) & "B"): s = s & Ap(f8)
    Err.Clear: a = Empty: a = Split(t, "a", 2, vbTextCompare): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1.5, 1): s = s & Ap(v)
    s = s & Ap(t)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "Xab,A,": u = "aB "
    Err.Clear: v = Empty: v = String(2.5, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "AbaB", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 0, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), ";"): s = s & Ap(v)
    Err.Clear: Mid(t, 10) = "AbXb": s = s & Ap(t)
    s = s & Ap(t)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = " bc b": u = ("x" & ChrW(233))
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    Err.Clear: Mid(t, 3, 2.5) = ("c1ab" & ChrW(201)): s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    s = s & Ap(t)
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aBa" & ChrW(223) & ",x"): u = ""
    Err.Clear: v = Empty: v = Replace(t, u, ("Ab" & ChrW(201) & " "), 0, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 0, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = LCase(t): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: Mid(t, 2) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 5): s = s & Ap(v)
    s = s & Ap(t)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = (ChrW(223) & "BA")
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(100): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(201) & "A" & ChrW(233) & "xaBA"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(201) & "XAb  x-aB"): s = s & Ap(f8)
    Err.Clear: Mid(t, -1) = u: s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    s = s & Ap(t)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aX11": u = "AbB,"
    Err.Clear: Mid(t, -1) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "aBAb"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(100, t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(3, 321): s = s & Ap(v)
    s = s & Ap(t)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = " ,,AaAbXaba aBc": u = "-1"
    Err.Clear: v = Empty: v = Left$(t, 3): s = s & Ap(v)
    Err.Clear: Mid(t, 1.5) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Replace(t, u, "1c", 1, 1): s = s & Ap(v)
    Err.Clear: Mid(t, 3) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(3, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: f8 = ("AbaB" & ChrW(233) & "abBaB,a"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "XX": s = s & Ap(f8)
    s = s & Ap(t)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(223) & "x" & ChrW(223)): u = ("caB" & ChrW(233))
    Err.Clear: v = Empty: v = Mid(t, 1, 5): s = s & Ap(v)
    Err.Clear: Mid(t, 5) = "aBA": s = s & Ap(t)
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    Err.Clear: Mid$(t, 5) = u: s = s & Ap(t)
    Err.Clear: Mid$(t, 0) = u: s = s & Ap(t)
    Err.Clear: Mid$(t, 2) = u: s = s & Ap(t)
    s = s & Ap(t)
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ",": u = "aBc"
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 2.5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 1): s = s & Ap(v)
    Err.Clear: LSet t = ("Ab" & ChrW(223) & "bb" & ChrW(201)): s = s & Ap(t)
    Err.Clear: f8 = "Ab": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Left(t, 1): s = s & Ap(v)
    s = s & Ap(t)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aBXaBaBa X aB": u = "AbB"
    Err.Clear: Mid(t, 5, 1) = ("a" & ChrW(233) & ";" & ChrW(223)): s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "xax"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2.5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    Err.Clear: LSet t = ("1" & ChrW(223) & "xc-x" & ChrW(201) & ChrW(223) & "b" & ChrW(223) & "cx"): s = s & Ap(t)
    s = s & Ap(t)
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ""
    Err.Clear: Mid(t, 2.5) = "Bb": s = s & Ap(t)
    Err.Clear: f8 = "": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(223) & "Xb"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStr(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 128): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "1A", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), (ChrW(233) & "A")): s = s & Ap(v)
    s = s & Ap(t)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(201) & ChrW(223) & "c" & ChrW(223) & ";Xa" & ChrW(223) & ChrW(201)): u = ""
    Err.Clear: v = Empty: v = Left(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 1.5): s = s & Ap(v)
    Err.Clear: Mid(t, 0) = "Ab1": s = s & Ap(t)
    Err.Clear: v = Empty: v = String(3, 321): s = s & Ap(v)
    Err.Clear: Mid(t, 1.5) = (ChrW(233) & "X-"): s = s & Ap(t)
    s = s & Ap(t)
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (" bax;X" & ChrW(233) & "abXaBx"): u = "ab"
    Err.Clear: RSet t = "-B1-ab,c;ab bbx": s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(2, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), (ChrW(223) & "-"), True, vbTextCompare): s = s & Ap(v)
    Err.Clear: Mid$(t, 2.5) = u: s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    s = s & Ap(t)
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "A,,AbaBaAb": u = ("B" & ChrW(233) & "x")
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = LenB(t): s = s & Ap(v)
    Err.Clear: RSet t = ("cx" & ChrW(223) & "XA-ac"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(0): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    s = s & Ap(t)
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aBBaB-" & ChrW(201) & ";aBc" & ChrW(201) & " ,"): u = (ChrW(233) & "abab")
    Err.Clear: v = Empty: v = Mid(t, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1.5): s = s & Ap(v)
    Err.Clear: f8 = "": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "aB": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Replace(t, u, "abAba", 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbTextCompare): s = s & Ap(v)
    s = s & Ap(t)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & "1B" & ChrW(223) & "X;" & ChrW(201)): u = "A"
    Err.Clear: v = Empty: v = InStr(10, t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, ("ab" & ChrW(201)), 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), (ChrW(233)), False): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "aB-", 1, 100): s = s & Ap(v)
    s = s & Ap(t)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ""
    Err.Clear: RSet t = (ChrW(233) & " " & ChrW(201) & ChrW(201) & "X"): s = s & Ap(t)
    Err.Clear: v = Empty: v = String(-1, "ab"): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(10, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "X"): s = s & Ap(v)
    s = s & Ap(t)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(223) & "c" & ChrW(223) & "," & ChrW(223) & "Ab Ab"): u = (ChrW(233) & ";")
    Err.Clear: Mid(t, 3) = (ChrW(223) & "aa"): s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, u, -1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(2): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(100, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    s = s & Ap(t)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(223) & "abaB1Abab1 1,"): u = "AaBc"
    Err.Clear: v = Empty: v = Mid(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(3, "ab"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    s = s & Ap(t)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("a-," & ChrW(223) & "X-11" & ChrW(233)): u = ("X" & ChrW(233))
    Err.Clear: v = Empty: v = String(-1, 321): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3): s = s & Ap(v)
    Err.Clear: RSet t = ("AbXX" & ChrW(233) & "x" & ChrW(223) & ChrW(201) & ";B-aB" & ChrW(201)): s = s & Ap(t)
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "x", 1, 0): s = s & Ap(v)
    s = s & Ap(t)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(201) & "XXcb-xAbAxab"): u = ""
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2.5): s = s & Ap(v)
    Err.Clear: RSet t = ",,Ab": s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(1, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    s = s & Ap(t)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = (";" & ChrW(233))
    Err.Clear: v = Empty: v = StrConv(t, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 3): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(233) & "A" & ChrW(223) & ChrW(223) & "aB1-aBxX"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(-1, t, u): s = s & Ap(v)
    s = s & Ap(t)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "bb ": u = ""
    Err.Clear: v = Empty: v = StrConv(t, 3): s = s & Ap(v)
    Err.Clear: f8 = ("X" & ChrW(233) & ";" & ChrW(201) & "Ba"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("a;" & ChrW(201)): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, (ChrW(223)), 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(0, t, u, 1): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 0): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    s = s & Ap(t)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("bA-aba1" & ChrW(223) & "A," & ChrW(233)): u = "b,ab"
    Err.Clear: v = Empty: v = Space(10): s = s & Ap(v)
    Err.Clear: Mid(t, 3) = (ChrW(223) & " 1ab"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(1.5, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, -1): s = s & Ap(v)
    Err.Clear: LSet t = ("x" & ChrW(201) & "1XXX;" & ChrW(223) & ";cacab"): s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    s = s & Ap(t)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "xXa-": u = ""
    Err.Clear: v = Empty: v = Right$(t, 1.5): s = s & Ap(v)
    Err.Clear: Mid(t, 5) = "- ": s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: Mid(t, 1.5) = u: s = s & Ap(t)
    Err.Clear: Mid(t, -1) = "xab": s = s & Ap(t)
    Err.Clear: Mid(t, 0, 5) = "X": s = s & Ap(t)
    s = s & Ap(t)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ("ab" & ChrW(223))
    Err.Clear: v = Empty: v = LenB(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = LenB(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "ab", 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1.5, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3): s = s & Ap(v)
    s = s & Ap(t)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "c": u = "aa"
    Err.Clear: v = Empty: v = StrConv(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, (",Ab" & ChrW(223)), 1, -1, 1): s = s & Ap(v)
    Err.Clear: Mid(t, 2.5) = u: s = s & Ap(t)
    Err.Clear: Mid(t, 3) = u: s = s & Ap(t)
    s = s & Ap(t)
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("1b" & ChrW(233) & ChrW(233) & ChrW(233) & "A" & ChrW(201) & "-" & ChrW(233) & ChrW(233) & "x"): u = ""
    Err.Clear: f8 = "a;ca-c": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("1" & ChrW(223)): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 3, 2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(0, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2.5, 10): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(233) & "cb"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (" baX" & ChrW(233) & "aBbB;"): s = s & Ap(f8)
    Err.Clear: Mid$(t, 2) = u: s = s & Ap(t)
    s = s & Ap(t)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aB,AbB" & ChrW(201) & " ;-"): u = ""
    Err.Clear: LSet t = "XaB,A;,": s = s & Ap(t)
    Err.Clear: v = Empty: v = Replace(t, u, "", 10, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2.5, -1): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, " "): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: Mid(t, 2.5) = "AX": s = s & Ap(t)
    s = s & Ap(t)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("bca" & ChrW(223) & "aaB" & ChrW(223) & " A"): u = (ChrW(201))
    Err.Clear: v = Empty: v = Right(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "aB", 10, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 3, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = LTrim(t): s = s & Ap(v)
    Err.Clear: Mid$(t, 100) = u: s = s & Ap(t)
    s = s & Ap(t)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "abb": u = (ChrW(223) & "AB")
    Err.Clear: v = Empty: v = Replace(t, u, "", 3, -1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(100, 321): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "-"): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: RSet t = "1bxAbab": s = s & Ap(t)
    s = s & Ap(t)
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "-x1bBXBb": u = "aBX"
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), ""): s = s & Ap(v)
    s = s & Ap(t)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & "1AbXbA"): u = ""
    Err.Clear: v = Empty: v = Right$(t, 1.5): s = s & Ap(v)
    Err.Clear: LSet t = "xaBab-X": s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 2): s = s & Ap(v)
    Err.Clear: LSet t = "XabXAabaBcabB": s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 2): s = s & Ap(v)
    Err.Clear: LSet t = ",c": s = s & Ap(t)
    s = s & Ap(t)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aAbA" & ChrW(233) & "X"): u = ""
    Err.Clear: v = Empty: v = Left$(t, 1.5): s = s & Ap(v)
    Err.Clear: f8 = ("AbXAb," & ChrW(233) & "1aBc1Ab"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(223) & "a" & ChrW(233) & "BaBX;"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStr(3, t, u, 1): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(201) & "ab" & ChrW(233) & "Ab-- B "): s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 100, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), ""): s = s & Ap(v)
    s = s & Ap(t)
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "X cca ": u = ""
    Err.Clear: v = Empty: v = InStr(100, t, u, 0): s = s & Ap(v)
    Err.Clear: Mid(t, 2.5) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, 2.5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 1, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(-1, 321): s = s & Ap(v)
    s = s & Ap(t)
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "Abb,-Xab,;": u = ""
    Err.Clear: RSet t = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(3, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    s = s & Ap(t)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aBAcxX;Ab": u = "-"
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3 + 64): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(-1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 2): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: Mid(t, 100) = u: s = s & Ap(t)
    s = s & Ap(t)
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = "X1"
    Err.Clear: RSet t = ("Ab" & ChrW(201) & ChrW(223) & "aBabab1"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, 1.5): s = s & Ap(v)
    Err.Clear: Mid(t, 1) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    s = s & Ap(t)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "-1,cxab,B ": u = ";;"
    Err.Clear: v = Empty: v = Mid(t, 2): s = s & Ap(v)
    Err.Clear: Mid(t, 1) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Replace(t, u, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 64): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(1.5, 65): s = s & Ap(v)
    s = s & Ap(t)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("c-" & ChrW(233)): u = "ab;1"
    Err.Clear: Mid$(t, 3) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = String(100, 321): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 100): s = s & Ap(v)
    Err.Clear: Mid(t, 2.5) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Replace(t, u, "", 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "BAb"): s = s & Ap(v)
    s = s & Ap(t)
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("x A1aaAc" & ChrW(233) & ChrW(233) & "1ab"): u = "AA"
    Err.Clear: v = Empty: v = StrConv(t, vbUpperCase): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: Mid(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(2.5, t, u): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(201) & "-A 1xaBAabab"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("-cabab" & ChrW(223) & ChrW(223) & "a"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Replace(t, u, "abaB1", 1, 3): s = s & Ap(v)
    s = s & Ap(t)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("ab ,Xc" & ChrW(233) & "AAaAbAbaB"): u = "ab"
    Err.Clear: v = Empty: v = InStr(-1, t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(100, t, u, 1): s = s & Ap(v)
    Err.Clear: Mid$(t, 1.5) = u: s = s & Ap(t)
    Err.Clear: Mid(t, 2.5, -1) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Replace(t, u, "XA", 10, -1, 1): s = s & Ap(v)
    s = s & Ap(t)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (";b 1a" & ChrW(223) & "aXab"): u = ""
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: RSet t = "Xx aa-a,": s = s & Ap(t)
    Err.Clear: f8 = ("axBx1X" & ChrW(201)): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(233) & "xa"): s = s & Ap(f8)
    Err.Clear: Mid(t, 2.5) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    s = s & Ap(t)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(223) & "aBA," & ChrW(201)): u = (ChrW(201) & " B")
    Err.Clear: f8 = "a": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "": s = s & Ap(f8)
    Err.Clear: RSet t = ("B" & ChrW(201) & ";aBBXxxx1"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "b", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1): s = s & Ap(v)
    Err.Clear: f8 = "caXaB": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(223) & "X"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Asc(t): s = s & Ap(v)
    s = s & Ap(t)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "xabaa": u = ""
    Err.Clear: RSet t = ("BX" & ChrW(201)): s = s & Ap(t)
    Err.Clear: v = Empty: v = Space(10): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(2.5, t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: Mid(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, 1): s = s & Ap(v)
    Err.Clear: Mid(t, 0, 100) = (ChrW(233) & "Ax"): s = s & Ap(t)
    s = s & Ap(t)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = " ": u = ""
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, (ChrW(201) & "BAb"), 2, -1, 1): s = s & Ap(v)
    Err.Clear: RSet t = ("B," & ChrW(223) & ChrW(223) & "AbX aBAx" & ChrW(233) & "bAAb"): s = s & Ap(t)
    Err.Clear: a = Empty: a = Split(t, u, -1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(223) & " ;"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "1": s = s & Ap(f8)
    Err.Clear: v = Empty: v = LenB(t): s = s & Ap(v)
    s = s & Ap(t)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(223) & ChrW(223) & "c " & ChrW(201) & ChrW(223) & "Abc"): u = "X"
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(233) & "baBB" & ChrW(201)): s = s & Ap(t)
    Err.Clear: RSet t = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Left(t, 2.5): s = s & Ap(v)
    Err.Clear: RSet t = ("b" & ChrW(223) & ChrW(201)): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    s = s & Ap(t)
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "xaBAb": u = (ChrW(223) & "aX")
    Err.Clear: v = Empty: v = Mid(t, 2.5, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(223) & " bb1aAx"): s = s & Ap(t)
    Err.Clear: Mid(t, 2, 10) = ("XX" & ChrW(233) & " "): s = s & Ap(t)
    Err.Clear: v = Empty: v = Left$(t, 1): s = s & Ap(v)
    s = s & Ap(t)
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("ab  aBBcB" & ChrW(223)): u = ""
    Err.Clear: a = Empty: a = Split(t): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Len(t): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 10, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 64): s = s & Ap(v)
    s = s & Ap(t)
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aBAb,aB;Ababa": u = "b,ab"
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left(t, 0): s = s & Ap(v)
    Err.Clear: LSet t = "1A": s = s & Ap(t)
    Err.Clear: Mid$(t, 10) = u: s = s & Ap(t)
    Err.Clear: RSet t = "aAb": s = s & Ap(t)
    s = s & Ap(t)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("ab1" & ChrW(223)): u = ""
    Err.Clear: LSet t = (ChrW(233) & ChrW(201) & ";;-aBB-AbAB"): s = s & Ap(t)
    Err.Clear: Mid$(t, 1) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 10, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), ""): s = s & Ap(v)
    Err.Clear: Mid(t, 100, 1.5) = ",xa": s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 0): s = s & Ap(v)
    s = s & Ap(t)
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("- " & ChrW(223) & ChrW(223) & "A1B" & ChrW(201) & "1Ababa"): u = ""
    Err.Clear: Mid$(t, 2) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 0): s = s & Ap(v)
    Err.Clear: f8 = ("," & ChrW(201) & "baB1"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "abaBab": s = s & Ap(f8)
    Err.Clear: v = Empty: v = StrConv(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "", False): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    s = s & Ap(t)
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aB1-;aB;1 1" & ChrW(223) & "B-"): u = ";1"
    Err.Clear: v = Empty: v = Mid(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(100, 65): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = LenB(t): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(233) & "abX1" & ChrW(223) & "B"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "aB;": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, -1): s = s & Ap(v)
    s = s & Ap(t)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "; ": u = (ChrW(233) & ChrW(233))
    Err.Clear: Mid$(t, 10) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = String(2, ""): s = s & Ap(v)
    Err.Clear: Mid(t, 3) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, -1): s = s & Ap(v)
    Err.Clear: RSet t = "B;": s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 3 + 64): s = s & Ap(v)
    s = s & Ap(t)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (",bAAb" & ChrW(201) & "aBAb" & ChrW(233) & ChrW(223) & "A"): u = ""
    Err.Clear: a = Empty: a = Split(t): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "aaBc"): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(5, t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbTextCompare): s = s & Ap(v)
    Err.Clear: Mid(t, 100) = "-": s = s & Ap(t)
    s = s & Ap(t)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("X-" & ChrW(233) & "-1X;"): u = ";"
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1): s = s & Ap(v)
    s = s & Ap(t)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(201) & "abXabX" & ChrW(233) & ChrW(223) & "XaB" & ChrW(223) & " c"): u = ""
    Err.Clear: v = Empty: v = Right(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "AbB", 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "", 100, -1, 1): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    s = s & Ap(t)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "AxabAc,BaBXx": u = " "
    Err.Clear: v = Empty: v = Space(2): s = s & Ap(v)
    Err.Clear: f8 = ("c," & ChrW(223)): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ",": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 2.5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 100, 1): s = s & Ap(v)
    s = s & Ap(t)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "-B": u = "Ab"
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 100): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbUpperCase): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1, 2): s = s & Ap(v)
    s = s & Ap(t)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "--Xab,X": u = "aBX;"
    Err.Clear: v = Empty: v = StrConv(t, 64): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 2.5): s = s & Ap(v)
    Err.Clear: f8 = (";aBaBaB" & ChrW(223) & "b  a" & ChrW(233)): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(201) & "xA" & ChrW(233) & " ;;X"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStrRev(t, u, 0, 1): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, "a", 2, vbTextCompare): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: Mid(t, 2.5) = "c;": s = s & Ap(t)
    s = s & Ap(t)
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (" aBX" & ChrW(201)): u = "A"
    Err.Clear: f8 = "AbcB": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Right$(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(1, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(5): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    s = s & Ap(t)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (",  cAaB-" & ChrW(223) & ChrW(223) & "B"): u = ";"
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), (ChrW(233) & "aB"), False): s = s & Ap(v)
    Err.Clear: Mid(t, 100, 100) = "": s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, -1, 10): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    s = s & Ap(t)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aBB," & ChrW(233) & "aB1 "): u = ""
    Err.Clear: Mid(t, 100, 0) = ("Bx" & ChrW(223) & "ab"): s = s & Ap(t)
    Err.Clear: v = Empty: v = InStr(3, t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "a", 1, 2.5): s = s & Ap(v)
    Err.Clear: LSet t = (";;aBc" & ChrW(223) & "xc" & ChrW(233) & "-a;" & ChrW(223) & "aAb-"): s = s & Ap(t)
    Err.Clear: v = Empty: v = Mid(t, 3, 2): s = s & Ap(v)
    Err.Clear: RSet t = "": s = s & Ap(t)
    s = s & Ap(t)
    C149 = s
End Function
Private Function C150() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("," & ChrW(233) & "; ,aB-" & ChrW(223) & ChrW(233) & ChrW(201) & ChrW(223) & "B"): u = ("ab" & ChrW(201))
    Err.Clear: v = Empty: v = Replace(t, u, "XAb", 1.5): s = s & Ap(v)
    Err.Clear: Mid$(t, 2) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = UCase(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), (ChrW(233) & "a")): s = s & Ap(v)
    s = s & Ap(t)
    C150 = s
End Function
Private Function C151() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aB": u = ("x" & ChrW(223))
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: Mid(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = String(0, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, -1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(3, u): s = s & Ap(v)
    Err.Clear: RSet t = "AbAb": s = s & Ap(t)
    s = s & Ap(t)
    C151 = s
End Function
Private Function C152() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "1BXbx-x1;caB": u = (ChrW(233))
    Err.Clear: v = Empty: v = Mid(t, 100): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "-Ab"): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, "ab-", 1, 1): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(201) & "a a1"): s = s & Ap(t)
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    s = s & Ap(t)
    C152 = s
End Function
Private Function C153() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "Ab": u = (ChrW(201))
    Err.Clear: v = Empty: v = InStr(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "aB", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(2.5, ""): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: Mid(t, 2) = u: s = s & Ap(t)
    s = s & Ap(t)
    C153 = s
End Function
Private Function C154() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("-,x " & ChrW(223) & "Bb"): u = "x1"
    Err.Clear: v = Empty: v = InStrRev(t, u, 1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = String(10, "ab"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "", False): s = s & Ap(v)
    Err.Clear: Mid$(t, 0) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = StrConv(t, 1): s = s & Ap(v)
    Err.Clear: f8 = "bab": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ",bB": s = s & Ap(f8)
    s = s & Ap(t)
    C154 = s
End Function
Private Function C155() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "bxaaBXx": u = "-"
    Err.Clear: v = Empty: v = Right$(t, 100): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(3): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 3): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: Mid(t, 0) = "a": s = s & Ap(t)
    Err.Clear: v = Empty: v = Len(t): s = s & Ap(v)
    s = s & Ap(t)
    C155 = s
End Function
Private Function C156() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("cb" & ChrW(201) & ",a1" & ChrW(233)): u = (ChrW(233))
    Err.Clear: v = Empty: v = InStrRev(t, u, 100, 1): s = s & Ap(v)
    Err.Clear: Mid$(t, 1) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), (ChrW(223) & "1")): s = s & Ap(v)
    Err.Clear: v = Empty: v = Left$(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: Mid(t, 10) = ("," & ChrW(201) & " X"): s = s & Ap(t)
    s = s & Ap(t)
    C156 = s
End Function
Private Function C157() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("Ab" & ChrW(201) & "-"): u = "c1"
    Err.Clear: Mid(t, 3, 2.5) = "-": s = s & Ap(t)
    Err.Clear: v = Empty: v = Filter(Split(t, u), "aaB", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), ""): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ""): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    Err.Clear: Mid(t, 10, 1.5) = (";x" & ChrW(223) & "aB"): s = s & Ap(t)
    Err.Clear: Mid(t, 5) = u: s = s & Ap(t)
    s = s & Ap(t)
    C157 = s
End Function
Private Function C158() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("bA1x1x" & ChrW(233)): u = "AA"
    Err.Clear: v = Empty: v = Mid(t, 3): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, -1, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, "|"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, " "), "1", False): s = s & Ap(v)
    Err.Clear: f8 = "ab1Ab": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "aB;": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Space(1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 128): s = s & Ap(v)
    s = s & Ap(t)
    C158 = s
End Function
Private Function C159() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aBaBaabxb 1xb": u = "c;"
    Err.Clear: v = Empty: v = InStr(t, u, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Trim(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 1.5, 1): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = LCase(t): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 5): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a): s = s & Ap(v)
    s = s & Ap(t)
    C159 = s
End Function
Private Function C160() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("ac-" & ChrW(201) & "abx"): u = "Bb;"
    Err.Clear: v = Empty: v = Right$(t, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 2): s = s & Ap(v)
    Err.Clear: Mid(t, 3) = u: s = s & Ap(t)
    Err.Clear: Mid(t, 0, 2) = "bAX": s = s & Ap(t)
    Err.Clear: Mid(t, 10) = u: s = s & Ap(t)
    Err.Clear: v = Empty: v = AscW(t): s = s & Ap(v)
    s = s & Ap(t)
    C160 = s
End Function
Private Function C161() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("cc" & ChrW(223)): u = "aBXB"
    Err.Clear: v = Empty: v = StrConv(t, 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), ","): s = s & Ap(v)
    Err.Clear: f8 = "b BabxAbAb": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("a" & ChrW(223) & " -cac"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = String(1.5, ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "", True, vbTextCompare): s = s & Ap(v)
    Err.Clear: Mid$(t, 3) = u: s = s & Ap(t)
    s = s & Ap(t)
    C161 = s
End Function
Private Function C162() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aBB" & ChrW(223)): u = ""
    Err.Clear: Mid(t, 5) = "b;": s = s & Ap(t)
    Err.Clear: v = Empty: v = InStrRev(t, u, -1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1.5, 1.5): s = s & Ap(v)
    Err.Clear: RSet t = (ChrW(223) & "XA " & ChrW(201) & "Ab"): s = s & Ap(t)
    Err.Clear: RSet t = (";aB-" & ChrW(201) & "a" & ChrW(223) & ",;aB,"): s = s & Ap(t)
    s = s & Ap(t)
    C162 = s
End Function
Private Function C163() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("aB" & ChrW(201) & "ab" & ChrW(223) & "BA1-A" & ChrW(201) & "b"): u = "1"
    Err.Clear: v = Empty: v = Right(t, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 1.5, 1): s = s & Ap(v)
    Err.Clear: f8 = "aAaB": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (ChrW(233) & ChrW(201) & "ax"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 10, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1.5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 4): s = s & Ap(v)
    s = s & Ap(t)
    C163 = s
End Function
Private Function C164() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "": u = ""
    Err.Clear: f8 = "aB": s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = (";aX" & ChrW(233) & "b-b"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 3, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right$(t, 1.5): s = s & Ap(v)
    Err.Clear: f8 = ("-" & ChrW(201) & "aB-BbAb" & ChrW(233) & ";"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("Abb1," & ChrW(223) & "Ab"): s = s & Ap(f8)
    Err.Clear: v = Empty: v = InStrRev(t, u, 10, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u, 0): s = s & Ap(v)
    s = s & Ap(t)
    C164 = s
End Function
Private Function C165() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("xAbc" & ChrW(201) & ChrW(223) & ChrW(233)): u = ""
    Err.Clear: v = Empty: v = InStr(1.5, t, u): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(201) & "cAbab-"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = "acaBAb": s = s & Ap(f8)
    Err.Clear: v = Empty: v = Mid(t, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Asc(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Space(1): s = s & Ap(v)
    s = s & Ap(t)
    C165 = s
End Function
Private Function C166() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "abxX": u = (ChrW(223) & ChrW(201) & ChrW(233))
    Err.Clear: v = Empty: v = Filter(Split(t, ","), "", False): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), " "): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(t, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbProperCase): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, 8): s = s & Ap(v)
    Err.Clear: f8 = (ChrW(223) & ChrW(201) & "ab x"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("XAc" & ChrW(201) & "xaa"): s = s & Ap(f8)
    s = s & Ap(t)
    C166 = s
End Function
Private Function C167() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = "aBB-xx-;B": u = (ChrW(233))
    Err.Clear: v = Empty: v = Left(t, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 1.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Right(t, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Filter(Split(t, u), ""): s = s & Ap(v)
    Err.Clear: v = Empty: v = LenB(t): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, ","): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, ""): s = s & Ap(v)
    s = s & Ap(t)
    C167 = s
End Function
Private Function C168() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = ("b-" & ChrW(233) & "ABabaB,c" & ChrW(223)): u = "AA"
    Err.Clear: v = Empty: v = Right(t, 2.5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 3, -1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Len(t): s = s & Ap(v)
    Err.Clear: v = Empty: v = Mid(t, 10): s = s & Ap(v)
    Err.Clear: v = Empty: v = Replace(t, u, (ChrW(201) & "aB"), 1, -1, vbTextCompare): s = s & Ap(v)
    Err.Clear: f8 = ("A1" & ChrW(233) & ChrW(223) & "a B"): s = s & Ap(f8): LSet f8 = u: s = s & Ap(f8): RSet f8 = ("x" & ChrW(223) & "Abx-"): s = s & Ap(f8)
    s = s & Ap(t)
    C168 = s
End Function
Private Function C169() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Dim t As String, u As String, f8 As String * 8, a As Variant, i As Long
    t = (ChrW(233) & " AbaBaBx" & ChrW(233) & "Abcc"): u = ("," & ChrW(201))
    Err.Clear: v = Empty: v = InStr(-1, t, u, 0): s = s & Ap(v)
    Err.Clear: a = Empty: a = Split(t, u, 1): s = s & Ap(a)
    Err.Clear: v = Empty: v = Join(a, u): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStr(100, t, u, vbBinaryCompare): s = s & Ap(v)
    Err.Clear: v = Empty: v = StrConv(t, vbUpperCase): s = s & Ap(v)
    Err.Clear: v = Empty: v = InStrRev(t, u, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = RTrim(t): s = s & Ap(v)
    s = s & Ap(t)
    C169 = s
End Function
