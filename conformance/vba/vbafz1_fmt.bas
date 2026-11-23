Attribute VB_Name = "ModVbFz1Fmt"
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
Public Function VbFz1Fmt(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: VbFz1Fmt = C0()
    Case 1: VbFz1Fmt = C1()
    Case 2: VbFz1Fmt = C2()
    Case 3: VbFz1Fmt = C3()
    Case 4: VbFz1Fmt = C4()
    Case 5: VbFz1Fmt = C5()
    Case 6: VbFz1Fmt = C6()
    Case 7: VbFz1Fmt = C7()
    Case 8: VbFz1Fmt = C8()
    Case 9: VbFz1Fmt = C9()
    Case 10: VbFz1Fmt = C10()
    Case 11: VbFz1Fmt = C11()
    Case 12: VbFz1Fmt = C12()
    Case 13: VbFz1Fmt = C13()
    Case 14: VbFz1Fmt = C14()
    Case 15: VbFz1Fmt = C15()
    Case 16: VbFz1Fmt = C16()
    Case 17: VbFz1Fmt = C17()
    Case 18: VbFz1Fmt = C18()
    Case 19: VbFz1Fmt = C19()
    Case 20: VbFz1Fmt = C20()
    Case 21: VbFz1Fmt = C21()
    Case 22: VbFz1Fmt = C22()
    Case 23: VbFz1Fmt = C23()
    Case 24: VbFz1Fmt = C24()
    Case 25: VbFz1Fmt = C25()
    Case 26: VbFz1Fmt = C26()
    Case 27: VbFz1Fmt = C27()
    Case 28: VbFz1Fmt = C28()
    Case 29: VbFz1Fmt = C29()
    Case 30: VbFz1Fmt = C30()
    Case 31: VbFz1Fmt = C31()
    Case 32: VbFz1Fmt = C32()
    Case 33: VbFz1Fmt = C33()
    Case 34: VbFz1Fmt = C34()
    Case 35: VbFz1Fmt = C35()
    Case 36: VbFz1Fmt = C36()
    Case 37: VbFz1Fmt = C37()
    Case 38: VbFz1Fmt = C38()
    Case 39: VbFz1Fmt = C39()
    Case 40: VbFz1Fmt = C40()
    Case 41: VbFz1Fmt = C41()
    Case 42: VbFz1Fmt = C42()
    Case 43: VbFz1Fmt = C43()
    Case 44: VbFz1Fmt = C44()
    Case 45: VbFz1Fmt = C45()
    Case 46: VbFz1Fmt = C46()
    Case 47: VbFz1Fmt = C47()
    Case 48: VbFz1Fmt = C48()
    Case 49: VbFz1Fmt = C49()
    Case 50: VbFz1Fmt = C50()
    Case 51: VbFz1Fmt = C51()
    Case 52: VbFz1Fmt = C52()
    Case 53: VbFz1Fmt = C53()
    Case 54: VbFz1Fmt = C54()
    Case 55: VbFz1Fmt = C55()
    Case 56: VbFz1Fmt = C56()
    Case 57: VbFz1Fmt = C57()
    Case 58: VbFz1Fmt = C58()
    Case 59: VbFz1Fmt = C59()
    Case 60: VbFz1Fmt = C60()
    Case 61: VbFz1Fmt = C61()
    Case 62: VbFz1Fmt = C62()
    Case 63: VbFz1Fmt = C63()
    Case 64: VbFz1Fmt = C64()
    Case 65: VbFz1Fmt = C65()
    Case 66: VbFz1Fmt = C66()
    Case 67: VbFz1Fmt = C67()
    Case 68: VbFz1Fmt = C68()
    Case 69: VbFz1Fmt = C69()
    Case 70: VbFz1Fmt = C70()
    Case 71: VbFz1Fmt = C71()
    Case 72: VbFz1Fmt = C72()
    Case 73: VbFz1Fmt = C73()
    Case 74: VbFz1Fmt = C74()
    Case 75: VbFz1Fmt = C75()
    Case 76: VbFz1Fmt = C76()
    Case 77: VbFz1Fmt = C77()
    Case 78: VbFz1Fmt = C78()
    Case 79: VbFz1Fmt = C79()
    Case 80: VbFz1Fmt = C80()
    Case 81: VbFz1Fmt = C81()
    Case 82: VbFz1Fmt = C82()
    Case 83: VbFz1Fmt = C83()
    Case 84: VbFz1Fmt = C84()
    Case 85: VbFz1Fmt = C85()
    Case 86: VbFz1Fmt = C86()
    Case 87: VbFz1Fmt = C87()
    Case 88: VbFz1Fmt = C88()
    Case 89: VbFz1Fmt = C89()
    Case 90: VbFz1Fmt = C90()
    Case 91: VbFz1Fmt = C91()
    Case 92: VbFz1Fmt = C92()
    Case 93: VbFz1Fmt = C93()
    Case 94: VbFz1Fmt = C94()
    Case 95: VbFz1Fmt = C95()
    Case 96: VbFz1Fmt = C96()
    Case 97: VbFz1Fmt = C97()
    Case 98: VbFz1Fmt = C98()
    Case 99: VbFz1Fmt = C99()
    Case 100: VbFz1Fmt = C100()
    Case 101: VbFz1Fmt = C101()
    Case 102: VbFz1Fmt = C102()
    Case 103: VbFz1Fmt = C103()
    Case 104: VbFz1Fmt = C104()
    Case 105: VbFz1Fmt = C105()
    Case 106: VbFz1Fmt = C106()
    Case 107: VbFz1Fmt = C107()
    Case 108: VbFz1Fmt = C108()
    Case 109: VbFz1Fmt = C109()
    Case 110: VbFz1Fmt = C110()
    Case 111: VbFz1Fmt = C111()
    Case 112: VbFz1Fmt = C112()
    Case 113: VbFz1Fmt = C113()
    Case 114: VbFz1Fmt = C114()
    Case 115: VbFz1Fmt = C115()
    Case 116: VbFz1Fmt = C116()
    Case 117: VbFz1Fmt = C117()
    Case 118: VbFz1Fmt = C118()
    Case 119: VbFz1Fmt = C119()
    Case 120: VbFz1Fmt = C120()
    Case 121: VbFz1Fmt = C121()
    Case 122: VbFz1Fmt = C122()
    Case 123: VbFz1Fmt = C123()
    Case 124: VbFz1Fmt = C124()
    Case 125: VbFz1Fmt = C125()
    Case 126: VbFz1Fmt = C126()
    Case 127: VbFz1Fmt = C127()
    Case 128: VbFz1Fmt = C128()
    Case 129: VbFz1Fmt = C129()
    Case 130: VbFz1Fmt = C130()
    Case 131: VbFz1Fmt = C131()
    Case 132: VbFz1Fmt = C132()
    Case 133: VbFz1Fmt = C133()
    Case 134: VbFz1Fmt = C134()
    Case 135: VbFz1Fmt = C135()
    Case 136: VbFz1Fmt = C136()
    Case 137: VbFz1Fmt = C137()
    Case 138: VbFz1Fmt = C138()
    Case 139: VbFz1Fmt = C139()
    Case 140: VbFz1Fmt = C140()
    Case 141: VbFz1Fmt = C141()
    Case 142: VbFz1Fmt = C142()
    Case 143: VbFz1Fmt = C143()
    Case 144: VbFz1Fmt = C144()
    Case 145: VbFz1Fmt = C145()
    Case 146: VbFz1Fmt = C146()
    Case 147: VbFz1Fmt = C147()
    Case 148: VbFz1Fmt = C148()
    Case 149: VbFz1Fmt = C149()
    Case 150: VbFz1Fmt = C150()
    Case 151: VbFz1Fmt = C151()
    Case 152: VbFz1Fmt = C152()
    Case 153: VbFz1Fmt = C153()
    Case 154: VbFz1Fmt = C154()
    Case 155: VbFz1Fmt = C155()
    Case 156: VbFz1Fmt = C156()
    Case 157: VbFz1Fmt = C157()
    Case 158: VbFz1Fmt = C158()
    Case 159: VbFz1Fmt = C159()
    End Select
    Exit Function
EH:
    VbFz1Fmt = "#ERR " & Err.Number & " " & Err.Description
End Function
Private Function C0() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(1E-05, "+0.###)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-17)), "-#,#.0# kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(2.5899999999999998E-06, "\%", 4, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(68), "-#,##0.#E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(27, "#,##0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(73327), """ab"".0#E-0;\x00000.#;- ", 3, 2): s = s & Ap(v)
    C0 = s
End Function
Private Function C1() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(CDate(43), "-#E-0 kg;000.000", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(2762000000000000, "#,#.0E+0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-94.6332), """ab""?.##;$#,##0E+00%;""zero"" ", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(515.33), "mmm:AMPM"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Empty, "&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(486), "$#,#E+00\%;""ab""00000.0# kg;""zero"" ", 2, 3): s = s & Ap(v)
    C1 = s
End Function
Private Function C2() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur((-1.7)), "Long Date", 4, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-141439.473), "@", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("-3", "(0,.##E-0;;+0.0 kg;@", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-13)), "#,#.0#\%;$E-0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(3028000, "#,##0.##e+#;0,.00%", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-8275)), "(#,##0,,.0000E-0\%;+.000%;- "): s = s & Ap(v)
    C2 = s
End Function
Private Function C3() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$((-53.543), ">&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "00000.0#E+0;+.0#E+0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "\Q-qhhmm", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-62.962)), """ab""#,##0,,%", 5, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(35845, "+?", 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "$#,##0,,.0#%"): s = s & Ap(v)
    C3 = s
End Function
Private Function C4() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#1/1/2000#, "$00000.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.17, "AMPM.s:c h/AMPM", 2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(14, "?E+0;-?.###; ", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-97), "A/P ss-mmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(6.4, "#,##0.0#E+00;""ab""#0.0#e+# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "ww", 2): s = s & Ap(v)
    C4 = s
End Function
Private Function C5() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("Hello World", "$##0 kg; #.####); "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(" 12 ", "#,##0.00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "?.0#;; #.0#%;@", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(94610000000, "dddddd,c ddd n", 7, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(41750000000000000), "\Q"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/1899#, "n/AMPM", 7): s = s & Ap(v)
    C5 = s
End Function
Private Function C6() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(922337203685477.6, "(##0.000%;;0,.000;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("1e3", "\x##0.0# kg;-0,.000E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(5430, "mm,AM/PM,ttttt/a/p-a/p"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "Standard"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("(5)", "mmA/P.A/P", 4, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(34352, "!@@@@"): s = s & Ap(v)
    C6 = s
End Function
Private Function C7() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$((-4794), "000e+#;;-##0e+#;@", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.3, "\x?);;\x#,#.####;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(346.78), """ab""?.00E-0;; #,##0,,.0000;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("2147483648", "-##0.#e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("12:30", "ss.ss/q dm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.57, "@"): s = s & Ap(v)
    C7 = s
End Function
Private Function C8() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("12", "(#,##0,,.##", 7, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-537.96), "#.###e+#;-##0.0#e+#", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "Medium Date"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1E+400", """ab""00000e+#;;+0.0000E-0);@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(10, " 0,.0#e+#%;(#.0#E-0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, " .0#", 4): s = s & Ap(v)
    C8 = s
End Function
Private Function C9() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#12/31/1899#, "Long Date", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "(#%;$0\%;""zero"" ", 0, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "mmmm.mmmmm,s"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-3.3), "mms,ww-w", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("12:30", """ab""#,#.###e+# ""u""", 5): s = s & Ap(v)
    C9 = s
End Function
Private Function C10() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(22750000000000), "(.0#E-0\%;(#.000E+0\%", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(0.1), "+00000.0#; 000.0000 ""u"";- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-61.7), "nn", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-7.77), "&&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-87), "$#0.0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(43, "#.00", 6): s = s & Ap(v)
    C10 = s
End Function
Private Function C11() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-493894.916), "@@@@@@", 2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "+000.####%;\x#,##0.00;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/1899#, "General Number"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-6.7)), """ab""00000E+0 kg;;(00000);@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("&H10", "mmmmAM/PM,hh"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "\x#,##0,, ""u""", 5): s = s & Ap(v)
    C11 = s
End Function
Private Function C12() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-347993.013), "+?.##%;#,##0.000E+0;0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.99, """<""@"">"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Empty, "$.0#e+#\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-64.113), "Yes/No"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("True", "General Number"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "#,##0,,E+00"): s = s & Ap(v)
    C12 = s
End Function
Private Function C13() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$((-13), "(#,#.###E-0;$#,##0,, ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-0.5), "(@@@) @@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("12", "#,##0,,e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "+#.0#\%;#.0#e+#;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(3.2, "-0,.000E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-64), "-#0.###"): s = s & Ap(v)
    C13 = s
End Function
Private Function C14() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("", "#,##0.0# ""u""", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1139, "ss,w.yy-""T"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(269.68), "+.###E-0 ""u"";0E+0 kg", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "On/Off", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(900799999999999.9, "$##0.00 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "-?%;?.####", 1, 2): s = s & Ap(v)
    C14 = s
End Function
Private Function C15() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(Null, "000.0#E+0);\x##0.#### kg;0 ", 5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(126388.848, "#0.0#E+00 kg; ##0;0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-90.36), "ww am/pm/s"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("-0", "d"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(388183.587, "$00000%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-31820), """ab""#,#);.0#E+0)", 0, 3): s = s & Ap(v)
    C15 = s
End Function
Private Function C16() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(Null, "general number", 5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-240257.156), " #;\x0.0#%; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-50.3), "#,##0.0000 ""u"";""ab""?.# ""u"";""zero"" ", 6, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(32), "+#E-0;#0.####%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "000.##", 1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", """<""@"">"""): s = s & Ap(v)
    C16 = s
End Function
Private Function C17() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(122636.404), "000E+00 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(40385.491, "c"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-97.5678)), "hh,cA/P:w.w", 6, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(10000000000000000), "Long Date"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-0.1)), "yyyy:ddww.m"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Null, "-000.0#e+# ""u"";$.0# ""u"";""zero"" ", 4): s = s & Ap(v)
    C17 = s
End Function
Private Function C18() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(Empty, " #,#.0000E+0%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-31.1), "m/c:q:w"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "wmm.A/P hh", 5, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.8, "mm.q.dddddd,mmmmm,w"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(5.67E-12, " 0,.####e+#;;-#,##0.00\%;@", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("12", " #0.###e+#);\x#,##0.0%"): s = s & Ap(v)
    C18 = s
End Function
Private Function C19() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("(5)", "#,##0,,e+#);0,.##e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-33574), "A/Pd", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("(5)", "Short Date", 2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "0,.00E+00 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(36, "$0,.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "?E+00;;(#.#E-0;@", 7, 3): s = s & Ap(v)
    C19 = s
End Function
Private Function C20() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$((-599.68), "am/pm-A/P/AM/PM:yy"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("", "w:ddddd", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "mmmmm/am/pm-ss"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(5.5660000000000006E-05), "-0.0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "!&&", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(7.672999999999999E-07, "!@@@@", 7): s = s & Ap(v)
    C20 = s
End Function
Private Function C21() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("Hello World", "&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "!@@@@", 7, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(9770, "AM/PM.hh"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#1/1/2000#, "(@@@) @@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#2/29/2000 12:00:00 PM#, "0.#", 4, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1e3", "\x#,##0.0000"): s = s & Ap(v)
    C21 = s
End Function
Private Function C22() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(593.88, "-#,##0.0e+#\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", " 00000E+00", 4, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(False, """ab""#,#", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, " ##0%", 7, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(True, "<@", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.8476, "##0.0#E+00%"): s = s & Ap(v)
    C22 = s
End Function
Private Function C23() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(DateSerial(2073, 2, 15), "-0,.0# kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "mmm/dddddd-nn/c", 7, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-2.5)), "mmddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.4E+38, "$00000.## ""u"";$#,#.####E-0); ", 0, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(29568), ".0", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-27354.992), " 0.###\%;-.0# ""u"""): s = s & Ap(v)
    C23 = s
End Function
Private Function C24() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("1.5", "a/p:ss.mmmmm-am/pm", 4, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-79), "$00000.##E-0;-?.0E+0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1.5, "s,am/pm", 3, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(72382.78, "+0,.0E+0", 5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-2.6), "am/pm,dddddd"): s = s & Ap(v)
    C24 = s
End Function
Private Function C25() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("1E+400", "-00000\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-10), "#0.#E-0\%", 1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#12/30/1899#, "!@@@@", 3, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.3, """ab""#,##0.###"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, " #,##0,,.0#e+# kg", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", """ab""000.0#E+0 ""u"";-##0.00E-0; "): s = s & Ap(v)
    C25 = s
End Function
Private Function C26() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(Empty, "(##0.0#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0, "Percent"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(0.85), "s.nn"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.00156, "@@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(19976, "+0,E+0; #,##0.00%;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "-0,.0#\%"): s = s & Ap(v)
    C26 = s
End Function
Private Function C27() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(Null, "00000.####\%;""ab""0.0#E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", " #,#.##E-0 kg;;+?.0#E+00);@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Empty, "d.ssmmmm dd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-39)), "0.000;(#0.0#e+# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(45), "Short Time", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(" ", "00000;;\x#0.####%;@", 2): s = s & Ap(v)
    C27 = s
End Function
Private Function C28() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("True", "nn/ttttt.A/P-a/p/mmmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.4E+38, "A/P ddddam/pm,n-ww"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-766.26), """ab""?\%;;$00000.0#e+# kg;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/2000#, "@@@", 1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("True", "(#,##0.0E-0 ""u"";0.0# kg;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(32767.5, """ab""#0.##", 0, 3): s = s & Ap(v)
    C28 = s
End Function
Private Function C29() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("abc", "##0.####e+#", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.005, "(0.####"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("Hello World", "-#,##0,,.0000E+00;#,#.###E+00; ", 0, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-19), "!&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-91), ">&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "<@"): s = s & Ap(v)
    C29 = s
End Function
Private Function C30() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(44), "general number"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#2/29/2000 12:00:00 PM#, "Standard", 3, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#2/29/2000 12:00:00 PM#, """ab""0E+0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "@@@@@@", 5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("Hello World", "General Number", 5, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(" ", " #,##0,,.###", 3): s = s & Ap(v)
    C30 = s
End Function
Private Function C31() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("1.5", "?.0#E+00\%;\x0.0000E-0%;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Empty, "\x#,#.0000e+# ""u"";-?.0000;- ", 3, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "\x0,.####"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", """ab""#0.0#e+# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.85, "\x.0#E+0\%;;-?.0000E+0);@", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "@"): s = s & Ap(v)
    C31 = s
End Function
Private Function C32() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(36604, "-#.0#E+00 kg", 1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/9999 11:59:59 PM#, "\x.000\%;-#,#.0#E-0 kg;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "ddddddnn:y-h"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-36.921)), "\x0E+00\%", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "dd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1.5", "w-n:dddd"): s = s & Ap(v)
    C32 = s
End Function
Private Function C33() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(True, "\x000\%;""ab""#,##0E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, """ab""#.0E+00%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("-3", """ab""0E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-5.3), "#0.#E-0;;\x000.0#E-0 ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-27.86), "-0,E+0\%;;-000.0#;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1.502E-10, "<@"): s = s & Ap(v)
    C33 = s
End Function
Private Function C34() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("Hello World", "+.# ""u"";#,##0,,.0#", 6, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CDate(2), "+#.000E-0%;(#,#e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(6.1, "0,.000E-0;-#,#", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(3), "\x##0.#%", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#2/29/2000 12:00:00 PM#, "#,#.000E+0;""ab""#.0e+#;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", """ab""000.0#;-#0.0#"): s = s & Ap(v)
    C34 = s
End Function
Private Function C35() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(9.787000000000002E-09, "-0.##E-0%;0E+0;0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "y.n:dddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-88), " 0,.#;00000.0#E-0; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "00000.0#E+00", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-281911.156), "General Number"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(100642.115, "#.000;$#,#E+00 ""u"";- "): s = s & Ap(v)
    C35 = s
End Function
Private Function C36() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(437850.172, "m.dddddd ddddd", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#6:00:00 AM#, """<""@"">"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(3.562E-05, "-#,#e+#\%;""ab""0E+00%", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-9.5), "##0E+00 kg;;;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(True, "#\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-31344), "Medium Time", 1, 3): s = s & Ap(v)
    C36 = s
End Function
Private Function C37() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(648.91, "#,##0,,.0#; ##0.0000e+# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/1899#, "Currency", 7, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "+0.###E+00\%", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-458.38), """T"",w \Q", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/2000#, """<""@"">"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.54, "<@", 0, 0): s = s & Ap(v)
    C37 = s
End Function
Private Function C38() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "$0,;$#0.0%;""zero"" ", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(320858.337, """ab""#,##0.#E+00 kg;(00000.####%; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1.5", "Medium Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(47720000000, "+#0E+00);;\x#E+00;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-12.55), "000.0#E+00;;""ab""#,##0e+# ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "a/p,myy/dddddd.yy", 2, 0): s = s & Ap(v)
    C38 = s
End Function
Private Function C39() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("abc", "\x#,##0.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-54.052), " 0,.0000E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", " #0.00E-0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(13, "\x#,#.00E+00\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "$#0.00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "000E+0"): s = s & Ap(v)
    C39 = s
End Function
Private Function C40() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("", "#.###e+#%; #0E-0%;0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/2000#, "000.####E-0 kg;""ab""#,##0,, ""u"";- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(8277, "\x#,##0,,e+#%;(?.###E+0 kg;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("", "$.0#e+#%", 3, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "standard", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-18.136), "-000;##0E+00%", 2, 0): s = s & Ap(v)
    C40 = s
End Function
Private Function C41() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(#12/31/9999 11:59:59 PM#, "AM/PM/ddmmmms"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1E-300, "@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(DateSerial(2012, 3, 24), "$#0.0 kg;;+000e+#;@", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-30657), " 0.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("false", "Scientific"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.4574, "ss.nn", 5): s = s & Ap(v)
    C41 = s
End Function
Private Function C42() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("Hello World", "dddd", 1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1.7, "s:""T"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-79), "\x#,##0,,\%;;0);@", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(" 12 ", "q"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-71.7956), "yyyy"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("0x10", "\x0E+0", 4): s = s & Ap(v)
    C42 = s
End Function
Private Function C43() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(5.4, """ab"".000 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Null, "\x#.0000E-0%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(False, """<""@"">"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(96, "n,mmmmm", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-0.6), "-0.0#;;(#,##0.0#%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(78100000000000000, "&&&"): s = s & Ap(v)
    C43 = s
End Function
Private Function C44() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(3.915E-08), " #,##0,,E-0;(000.0#\%;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-214801.513), "True/False"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(182.21, "#0.0000E-0;;000.0#E-0;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "+#,##0.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "mmmmm-am/pm", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "(#,##0,,E-0\%"): s = s & Ap(v)
    C44 = s
End Function
Private Function C45() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("2147483648", "Medium Date", 0, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "$#0.0#E+00 ""u""", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CDate(32368.3), """ab""#,#.###", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("Hello World", "Medium Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "Currency"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "$000e+#;$?E+0"): s = s & Ap(v)
    C45 = s
End Function
Private Function C46() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#12/31/9999 11:59:59 PM#, "#,##0.###E+0", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.278E+18, "Yes/No"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(9.509000000000001E-05, "&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "0.# ""u"";#0.#)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(342100000, "+000E+0", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-68.4), "&"): s = s & Ap(v)
    C46 = s
End Function
Private Function C47() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("x", "&&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "&&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.48, "$#e+# ""u"";;$.0000;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(" ", "#,#.###E-0);;000.##E+0);@", 0, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.5, "(?.0000\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-29735), "00000E+0 kg;;#0.0;@"): s = s & Ap(v)
    C47 = s
End Function
Private Function C48() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#12/31/1899#, "dd,mmmmm,yyyy-n:ddddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "##0E-0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-85.097)), "#0.##E+0", 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "#,##0.####"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.782, "(#0", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-94)), """T"""): s = s & Ap(v)
    C48 = s
End Function
Private Function C49() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("1,234", "(@@@) @@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-349773.597)), "#,##0,,.###e+# ""u"";#,##0.0E+0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "$000.0#E+0);-.000 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-5.7), """ab""?.#E+0 ""u"";;$?.####E-0\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-19)), "#0.##\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "Standard", 7): s = s & Ap(v)
    C49 = s
End Function
Private Function C50() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(0.74, " 000.0#E-0;;(#,##0,,.0# ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "AM/PM"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "mmm yyyy:ww", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "s-AMPM", 3, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(51670000, """ab""00000E+0%;;\x0,;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "#0.####E+0;-#,#.0 ""u""; "): s = s & Ap(v)
    C50 = s
End Function
Private Function C51() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(3.5), "d ttttt", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "AMPM/ddddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "#,#E+00", 2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-9.4)), "(@@@) @@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.114, "-?E-0 kg;""ab""000.0#E+00%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-18783), "mmm w.mmm"): s = s & Ap(v)
    C51 = s
End Function
Private Function C52() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(33451, """<""@"">"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("1e3", "\x.000E+0\%;\x00000.0#E-0\%;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(".5", "-0,.#E+0 kg", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(922337203685477.6, """ab""#E+0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-76)), "#,##0.0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(69), "<@"): s = s & Ap(v)
    C52 = s
End Function
Private Function C53() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur((-126142.652)), "$#,#.0# kg;;""ab""#,##0,,.00E+00\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(53), "mmm,\Qa/p.c"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(40), "ddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.04614, "(#.0#%;;(00000.0;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "(@@@) @@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("&H10", "&&&"): s = s & Ap(v)
    C53 = s
End Function
Private Function C54() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(CCur(352532.843), "-.0E+00", 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1E+300, "#,#.0#e+#;;$#.####E+00\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-15.764), "Percent"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "#,#", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#12/31/9999 11:59:59 PM#, ".0000 kg; #0.0#E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-39), "$#.0#E+0)"): s = s & Ap(v)
    C54 = s
End Function
Private Function C55() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-48), "$? kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("+4", "am/pm dddddd AMPMddd mmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-38702)), "y h n/ss"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(96, """ab""000.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(2.3, "0.0000E-0;.0000e+#; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(DateSerial(1999, 3, 7), "cdddd-h m/\Q"): s = s & Ap(v)
    C55 = s
End Function
Private Function C56() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("abc", ".0)", 7, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1E+400", "&&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", """T"".h/ttttt", 5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(0.804), "m/hh.ttttt-ttttt", 1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#6:00:00 AM#, "d,w"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("(5)", "Scientific", 3, 0): s = s & Ap(v)
    C56 = s
End Function
Private Function C57() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(True, "!@@@@", 2, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1.5, "True/False"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-74), "!@@@@", 0, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(251638.745, "$0,E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Null, "&", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.5, "0,.00E+0"): s = s & Ap(v)
    C57 = s
End Function
Private Function C58() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(8.9, "00000", 6, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-63.43), "$0,.0#E-0 kg", 0, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1.5, "#,#.0E+0", 1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-175.44), "(#,##0.#### kg;; 00000.00e+#\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "+##0E+00;;-.0#E-0;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-27.9197), "#,#;;#,##0.##E+0;@"): s = s & Ap(v)
    C58 = s
End Function
Private Function C59() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("Hello World", "yy"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "-00000.#", 0, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(344434.025, "y", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "-#0 kg;-000.## ""u""", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "!&&", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("3.0", "$#0.0#e+#);##0.000 ""u""", 1): s = s & Ap(v)
    C59 = s
End Function
Private Function C60() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("0", "?.0#E-0\%;""ab""0,.0#E-0; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "\x#,##0,,.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "a/p:""T""-AM/PM", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-74304.35), "Currency"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#2/29/2000 12:00:00 PM#, "00000E+0;$00000", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(8.177E-10, "00000.0#"): s = s & Ap(v)
    C60 = s
End Function
Private Function C61() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(True, "\x0,.0#E+00 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "000"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1e3", "Fixed"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-13648), ">&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(True, "On/Off"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CDate(43), "True/False"): s = s & Ap(v)
    C61 = s
End Function
Private Function C62() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur((-34)), "ss"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-7.7), "Short Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-50), "00000.0#E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-70.49)), "\x.0#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.493, """ab""0.#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "000e+#"): s = s & Ap(v)
    C62 = s
End Function
Private Function C63() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("abc", """ab""#,##0,,.0E-0;#,##0,,.00E+0);- ", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "m-AM/PM,ddd.w AM/PM"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("5%", """T"" w.ddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", """ab""##0.####)", 2, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "#,##0,,.0#\%;(#,#.###);- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "-#0; 0;""zero"" ", 5): s = s & Ap(v)
    C63 = s
End Function
Private Function C64() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("Hello World", "000.0#e+#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("$5", "am/pm.d"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-910.13), "@@@@@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("True", "-0.00E-0%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-9263), "s-mmmmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-699.91), "mmmmm.n"): s = s & Ap(v)
    C64 = s
End Function
Private Function C65() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-9316), "mm,a/p qy"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-2.3), "q/dddd-mmmmyy,am/pm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "\x00000.#E+00; #0.0000e+#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "-00000.###E+0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(134251.731, "#0.0e+#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "$.##E+00%"): s = s & Ap(v)
    C65 = s
End Function
Private Function C66() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-18), "yy"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-169.79)), "@@@@@@", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-41)), "00000.000 kg;;\x.00E+00 ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#2/29/2000 12:00:00 PM#, "$#,##0.0#E-0);00000.00E-0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "n:AM/PM"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "-#0.### kg"): s = s & Ap(v)
    C66 = s
End Function
Private Function C67() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("True", "\x#E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-973.83)), ">&", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(404496.083, "?E+0", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "AM/PM:am/pm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "+#,#.#### kg; #.###e+#\%;0 ", 6, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(207435.708, "##0.0#)", 2): s = s & Ap(v)
    C67 = s
End Function
Private Function C68() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(DateSerial(1990, 6, 11), "hh\QAMPM/m-dddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1E+300, "00000.0#E-0 ""u"";;-##0.###E+00 ""u"";@", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", """ab""#,#)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-65), " ?E+00 kg;0,E-0\%", 0, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, """ab""#,##0.0000);;00000.### ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "$000.000 ""u""", 0, 0): s = s & Ap(v)
    C68 = s
End Function
Private Function C69() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(True, "+?e+#; 0.###E-0;0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(227.4, """<""@"">"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", """ab""?.### kg; 000.#### kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(19377), "yy:ttttt:n.y"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(45632.97644), "Currency", 3, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.2, "$00000.##\%;\x.0#%", 5): s = s & Ap(v)
    C69 = s
End Function
Private Function C70() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-40), """ab""#,##0.0#E+0", 6, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-97.17), "On/Off"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "!@@@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(10000000000000000, "##0.##E+0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("2147483648", "dddddd,q", 1, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, """ab""#,#.0#E-0%;(0,.0E-0;- ", 1, 2): s = s & Ap(v)
    C70 = s
End Function
Private Function C71() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(464451.019, "-##0e+# kg;(000.###E-0 kg;0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(98295.023, "(000.0#;$?.####E-0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(65, ">&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1E+400", "00000.0#e+#)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("+4", "ttttt", 2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-40.51), "+000E+0 kg;;""ab""0,E+00%;@"): s = s & Ap(v)
    C71 = s
End Function
Private Function C72() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur((-98.5)), "$#,#E+00\%", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(2684, """ab""000.####e+#)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(216.85, "$000.####e+#;-.0#E+00 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("", "+000E+00)", 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "+0.00)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-16402), "#,##0.000E-0;#,##0.0#;- "): s = s & Ap(v)
    C72 = s
End Function
Private Function C73() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#12/31/9999 11:59:59 PM#, "hdddddd/yyyy:mmmmm,AMPM", 6, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-41.73), """ab""0,.000\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-21.256), "##0.#;\x##0.##E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Empty, "+0,.000E+00);\x000.###E-0);- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-3.4), "&&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(985.13, "+##0.00", 7): s = s & Ap(v)
    C73 = s
End Function
Private Function C74() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("+4", "a/p"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/1899#, """ab""?.0#E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "ss.y A/Pn.y", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.405674, " ?.0#e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(3.3, "\x#0.0#%;+#,##0\%; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "(@@@) @@@"): s = s & Ap(v)
    C74 = s
End Function
Private Function C75() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(5.7), "(.###E-0;;?.0#E-0\%;@", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "00000.0#%;;+#E+00 ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "(000.####e+#)", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(10000000000000000, """ab""##0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-32768.5), "#,##0.000E+0", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "(00000.00\%", 6, 0): s = s & Ap(v)
    C75 = s
End Function
Private Function C76() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$((-21), "(0.0#E+0;$00000.####E-0 kg;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-10770)), "!@@@@", 3, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "Yes/No"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "Medium Date", 2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("&O17", """T""mmmm:s/yy:w"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(" ", "mmm"): s = s & Ap(v)
    C76 = s
End Function
Private Function C77() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(Empty, "$##0E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/2000#, " ""u""", 4, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("Hello World", "am/pm h.ddd-q"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-766.86), "#0.####\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", " 000.000e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(233.91, "+#,#.00 kg;#0.###e+# ""u"""): s = s & Ap(v)
    C77 = s
End Function
Private Function C78() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(False, "AMPM yyyy y.AM/PM", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "\x000.000E+0%;""ab""#.#E-0;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(757.48), " 000.000E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-38.7), """ab""00000E+0;\x0.0#e+#", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "am/pm-ww/A/P"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(2.5), """T""AM/PM,\Q"): s = s & Ap(v)
    C78 = s
End Function
Private Function C79() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("-0", "(#,#.0;;$0.00e+# ""u"";@", 1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "##0E-0", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(" ", "(?.0E+0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("5%", "Short Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.5, "Standard"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "+##0.0000;#,##0.0#%"): s = s & Ap(v)
    C79 = s
End Function
Private Function C80() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$((-35425), "mmm.mmmmm""T""-ss", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("-3", "00000.0#E-0", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1.5, "True/False"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "0,.000E+00)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(" ", "000.#\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "\x#,##0.###)"): s = s & Ap(v)
    C80 = s
End Function
Private Function C81() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(87110000000000000, "?.000E+00\%", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(32767.5, "$#,##0e+#;\x.0#E+00; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.328696, "-##0.0#E+00\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(True, """ab""#.####E-0;#,#.#E+0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(0.492389), "dddddd/dddAMPMyy.y"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-91.207), "n/mmmm", 5, 3): s = s & Ap(v)
    C81 = s
End Function
Private Function C82() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("  7  ", "general number", 0, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "Percent"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-1018), "\x?.00 kg", 4, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1, "(@@@) @@@", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "Fixed"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(29382), "0.#E+0\%;;$0.##E+00 ""u"";@", 3): s = s & Ap(v)
    C82 = s
End Function
Private Function C83() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("Hello World", "+#,##0,,.0E+0);;\x0,.00;@", 5, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", "-#,##0,,.#)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "ddd/AM/PM,mm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "#,#.#e+#", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "Standard"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(40, "0,E+00%"): s = s & Ap(v)
    C83 = s
End Function
Private Function C84() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(False, "Short Time", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("(5)", "0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(92), "Fixed"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("$5", """ab""#,#E-0 ""u"";+##0.0#E+00\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(8.022E-11), "#,#.0000e+#; 000.0#E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "AM/PM/hh/\Q,mmm"): s = s & Ap(v)
    C84 = s
End Function
Private Function C85() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#12/31/9999 11:59:59 PM#, "\x?.#E+0 ""u"";;(#.## kg;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.5181, " 000.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/100#, "Long Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#12/31/9999 11:59:59 PM#, "#,##0,,.#E+0;-#,##0,,.#E+0", 6, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-0.5), "Percent"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(952.22, """ab""#,#.0# kg;+000.#E+0; "): s = s & Ap(v)
    C85 = s
End Function
Private Function C86() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("-0", "#,##0.0000%;$#,##0.000E+0\%;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.426824, "On/Off", 6, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(True, "-##0.0#E+00 ""u"";-#,##0E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(7736, "?.# kg;;-00000.##e+# ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("5%", """ab""##0.0#E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, ".####);(#.##e+#", 7): s = s & Ap(v)
    C86 = s
End Function
Private Function C87() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(False, "?.0# kg;;00000.0000 ""u"";@", 7, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/2000#, """ab""0,.0# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "mm-\Q,am/pm", 4, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-2.5), "yy", 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#6:00:00 AM#, "hh.A/P:y,AM/PM.ddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "-#.0#E+0 ""u""", 4): s = s & Ap(v)
    C87 = s
End Function
Private Function C88() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("&O17", "(000\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-55)), "A/P:mm-d", 4, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(13006), """ab""00000.0#E+0", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(0.58252), "mmmmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "#.###E+00%;;+#.##\%;@", 6, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-39761), "dd/nn"): s = s & Ap(v)
    C88 = s
End Function
Private Function C89() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("abc", "(00000.0#e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(2147483647.5, "AMPM,mm,w-AMPM"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(" ", " #,##0,,.#;-#,##0.#E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-11), "(##0E-0%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "True/False", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-6.7), "#,##0,, kg;#.####E+00 kg; "): s = s & Ap(v)
    C89 = s
End Function
Private Function C90() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-9655), "#0", 0, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.35165, "n,AMPM,yyyy", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "#,#.#E-0", 3, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-480.69), "ww"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-526)), "Percent", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.125, "(000.#### ""u"";;-00000;@"): s = s & Ap(v)
    C90 = s
End Function
Private Function C91() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(Null, "$000E+0 kg;\x00000E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(13805), "a/p"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "Percent"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1,234", "-#.0000; #.00); "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("$5", "(#,##0E-0;;""ab""#,##0,,.0#E-0 ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-60.7), "!&&"): s = s & Ap(v)
    C91 = s
End Function
Private Function C92() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("false", " ##0.0e+# kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("&O17", "q"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(202.68, "AM/PM"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(True, "standard"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#1/1/100#, "dddddd""T""/w.AMPM.""T"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#2/29/2000 12:00:00 PM#, "#,##0,,.#E-0;+##0.0# ""u"""): s = s & Ap(v)
    C92 = s
End Function
Private Function C93() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(91, "#,#e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(DateSerial(1918, 4, 24), "y,ttttt:ddd,ddddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("0x10", "Currency", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(27, " #0.0#e+#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(0.3), "hh", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#12/31/9999 11:59:59 PM#, "Long Date"): s = s & Ap(v)
    C93 = s
End Function
Private Function C94() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("Hello World", "0.0e+# kg;-#.# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.176E-12, "general number"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1, "(\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1,234", "?.0#E-0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-10)), "!&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("&H10", "ss/ddddh"): s = s & Ap(v)
    C94 = s
End Function
Private Function C95() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(CCur(0.15224), "?.#%;$0,E+0;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-13959.169), "+000.0#e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "$#,#.0e+# ""u"";;#.0#E+00;@", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-2.1), ">&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "e+# ""u""", 5, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(21301, "+#0E+00);;#,#E-0\%;@"): s = s & Ap(v)
    C95 = s
End Function
Private Function C96() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-70.71), "0,.####;0E+0", 3, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Null, "ss:y.AMPM,\Q:ss"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(0.5), "yyyy,s,dddddd-c mmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("2.5", "+#0.000E+00;0,.##E-0 kg", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("(5)", "+#,# kg;;+##0.####);@", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(DateSerial(1965, 11, 12), "+.#E-0;#,##0); ", 1): s = s & Ap(v)
    C96 = s
End Function
Private Function C97() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("32768", "yy,AMPM mmmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "0,.0E+00 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("3.0", "#.### ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(42, "$#0E+00%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "#,##0.###E-0;#0.#E+00)", 0, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(87370000000000000), "Long Date"): s = s & Ap(v)
    C97 = s
End Function
Private Function C98() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(81.64999999999999), "-#,#E+0\%;;\x#,##0.##%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/9999 11:59:59 PM#, "00000.00)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("0", "0,E-0;+#,##0,,.#E+0%; ", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/100#, "&", 0, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-35.594), "!&&", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "#.000;\x#0.0#e+#);- "): s = s & Ap(v)
    C98 = s
End Function
Private Function C99() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("+4", "\x#,##0,,E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/100#, "A/P:wttttt.\Q,am/pm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "##0.#\%;$0E+0 kg;0 ", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("  7  ", "##0.##E-0 kg", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "Scientific"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-32768.5), "A/P,dd"): s = s & Ap(v)
    C99 = s
End Function
Private Function C100() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("x", "s", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-2.8)), "#E+00 kg", 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(431055.604), "00000 kg", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-9.3), """T"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.23, "$#,##0,,.0E-0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("1e3", "+#.000;#,##0.0E+00)"): s = s & Ap(v)
    C100 = s
End Function
Private Function C101() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(0.05, "$#,#.0E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-93.21), "General Number"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(True, "dd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "+0", 0, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(72), "#,##0.##)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "a/p-A/P:A/P-mmh"): s = s & Ap(v)
    C101 = s
End Function
Private Function C102() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("5%", " #.0#e+#%;$#0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(9946, "+000.#%;0.0000); "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-13040), "ddd.hh dddddd.y.ddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(27346), "mmmm/AM/PM"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "On/Off", 4, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("12:30", "d", 2, 2): s = s & Ap(v)
    C102 = s
End Function
Private Function C103() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("Hello World", " #,##0,,.###e+#;##0.#E-0 ""u"";- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(387811.442), "-?E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "q-AMPM"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-67), " #,##0,,.000 kg;(#,##0 ""u""; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("", "#,#.0#e+# kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(55680.77207), "c:\Q:sAMPM/q"): s = s & Ap(v)
    C103 = s
End Function
Private Function C104() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("(5)", "\x00000.0# ""u"";;$00000.##E-0\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-53.087), "ydddmmmm:""T""", 5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-88781.001), ">&", 0, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("Hello World", " #,#.0E+00);+0, ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-243.14), "d h"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(354447.573, "-?.0000 kg"): s = s & Ap(v)
    C104 = s
End Function
Private Function C105() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-40.91), "$##0.###E-0 ""u"";;""ab""#0.000E+0;@", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "#E-0", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1.7030000000000002E-10, "$0,E-0 kg;E-0", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("-3", "+##0.###);#,##0,,.0#E+00;- ", 0, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(0.9), "+000.#E-0%;""ab""?E+0); ", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("1d2", "Fixed"): s = s & Ap(v)
    C105 = s
End Function
Private Function C106() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(#12/30/1899#, "@@@@@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1790000000000, """T""/yyyy,mmm/yy", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(8.996000000000001E-12, "Short Date"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("12:30", "ddddd/n mmmm-ddd,mmm", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Null, "@@@@@@", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#2/29/2000 12:00:00 PM#, "\x#,#.#)", 1): s = s & Ap(v)
    C106 = s
End Function
Private Function C107() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-38.98), " #,#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, """ab""#,##0.## kg", 0, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-3.8), "s:ss", 5, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(556.84), "+0,E+00 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-33.3)), "+000.0#);#,##0.0000E-0\%", 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-746.15)), "##0E+00 kg;+##0E+0 ""u"""): s = s & Ap(v)
    C107 = s
End Function
Private Function C108() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("12", "$#0.0#E-0);;+#,##0.###E+00%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(37465), """<""@"">"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.958841, " 00000.0#E-0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("3.0", "#,##0,,.0#e+#;#,##0.000E-0;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(934.23), "-?.0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "0.####E+00);;+00000.0000E+00 kg;@"): s = s & Ap(v)
    C108 = s
End Function
Private Function C109() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#12/31/1899#, """ab""#,##0,,.0#;-##0.### kg;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(376538.047, "(00000.####;""ab""##0.000E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "AMPM.A/P-d.a/p y"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1,234", ">&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(2.6, "am/pm:mm-mm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-80.1)), """ab""#,##0.0#E+00 kg"): s = s & Ap(v)
    C109 = s
End Function
Private Function C110() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-26603), "0.000e+#\%;;?.0#E+00%;@", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("  7  ", "yyyy/w-dddddd.am/pm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "-00000.000);+#0.#", 2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "a/p-ttttt"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-24.8), "\x#,#.0# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "<@"): s = s & Ap(v)
    C110 = s
End Function
Private Function C111() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#12/31/9999 11:59:59 PM#, "(0.0#E+00%;-#,##0%;0 ", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.053, " 00000.#;""ab""#,#.###"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-32768.5), "General Date"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "dd,yy", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-48), "#,##0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("-0", "Long Date", 3, 3): s = s & Ap(v)
    C111 = s
End Function
Private Function C112() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("abc", "cyy"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("", "Scientific"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(30248), "\Q-dd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/2000#, "\x.####; .##E-0;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("&O17", "&&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CDate(5293.8), "+##0 kg;000.####e+#\%", 0): s = s & Ap(v)
    C112 = s
End Function
Private Function C113() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(".5", "(#e+#);\x#0.###E-0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-62.84), "0.0000E-0 kg;0,;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(906.48), "e+# kg", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "yyyy-A/P/w dddddd-nn"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(21326), "dddddd.ddddd-yyyy-ddddd dddddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-198.3), "?.0000)"): s = s & Ap(v)
    C113 = s
End Function
Private Function C114() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(3.4, " #,##0.000E-0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, """ab""00000.000E+00\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(False, "#0.####E+0%", 1, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "w/n-AMPM", 6, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "#,##0.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-20.4)), "d", 4, 1): s = s & Ap(v)
    C114 = s
End Function
Private Function C115() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(Empty, "(#0.0#e+#;$00000.0;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-265729.929)), """ab""#0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(223907.885, "0)", 1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1.5, "!@@@@", 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#2/29/2000 12:00:00 PM#, "##0.0# kg;;+0e+#;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.975318, "0"): s = s & Ap(v)
    C115 = s
End Function
Private Function C116() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(123456.789, "(#,#.000", 7, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Empty, "$0.0#E+0 kg;-000.####E-0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.331E-08, """ab""000.00E+0 kg", 6, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-821.04)), "General Number"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-79.62)), "&&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, """ab""00000.0#e+#);-0,.0#E-0;0 ", 2): s = s & Ap(v)
    C116 = s
End Function
Private Function C117() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#1/1/100#, "0,.000\%;;?.0#\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "?e+#%;+##0E+0; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(74912.3583), "tttttc mmmm", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "(#,##0,,.000E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(115724.625, "w-ss", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("", "y:AMPM:ddddd.w:c"): s = s & Ap(v)
    C117 = s
End Function
Private Function C118() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(17, "0,.#E+00);.00e+#);- ", 4, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(63550.00000000001), "(#.00E+0 ""u"";$##0.00 ""u""; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(128.47), "&&&", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-31886)), "00000.0000E+0", 2, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "-.000e+#)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(DateSerial(2031, 11, 12), "#,#.0", 0, 3): s = s & Ap(v)
    C118 = s
End Function
Private Function C119() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(797, "am/pm", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("+4", "(000.0#E+0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "#e+#;##0.0#\%", 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.9, ">&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(6424, "\x#0;;$000e+# kg;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-54), "\x000.###e+# kg"): s = s & Ap(v)
    C119 = s
End Function
Private Function C120() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("", "Scientific"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "000.###e+#);(#0.0#E+00%;0 ", 3, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-61), "!@@@@", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "$#,##0,,.##E+00%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/100#, "(0.0#e+#;##0.#e+#)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, " #.0000E+0 kg;(#0.0# kg;- ", 5, 1): s = s & Ap(v)
    C120 = s
End Function
Private Function C121() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-68), "&", 6, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "\x#,##0E+00\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#2/29/2000 12:00:00 PM#, "nn/wdd ttttt"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-12.9536), "Short Time", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-233324.506), "Medium Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-340.99), " 00000.00E+0\%;$0.000e+#)"): s = s & Ap(v)
    C121 = s
End Function
Private Function C122() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-2.5), "ddddd/AMPM/ddddd.ddd/dddddd", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "w yyd", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "ttttt,AM/PM.dd/mmmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.26, "00000.0# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("12", " ##0.0#)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-285469.809)), "+000.0000;$?.00E+00)", 1, 2): s = s & Ap(v)
    C122 = s
End Function
Private Function C123() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-7.7), "Currency"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", " 000.00E+00\%", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("32768", """<""@"">""", 7, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(-10), " 000.##e+# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.53, "-#,#.#E-0\%", 2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("  7  ", "-0, kg"): s = s & Ap(v)
    C123 = s
End Function
Private Function C124() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("2.5", "-#0", 2, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "(000.### kg; 0.####E+00%;0 ", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(0.705502), "&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#2/29/2000 12:00:00 PM#, "!&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(410674.117, "hhssdd q", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "+0.# kg", 3): s = s & Ap(v)
    C124 = s
End Function
Private Function C125() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("12:30", "@@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1E-300, "-00000.#E+0);#,##0,, ""u""", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-41), "\x#.###;(#,##0,,.000e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-83.7425), "!&&", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "-000.0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.588419, "\x##0.0#", 6, 2): s = s & Ap(v)
    C125 = s
End Function
Private Function C126() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-8.4), " 000.##)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(496615.487, "+0,.0# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-2.2)), "$#\%;;(#,#.0#;@", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "ddd/mmmm h"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "w"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(922337203685477.6), "<@"): s = s & Ap(v)
    C126 = s
End Function
Private Function C127() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(0.887828), """ab""##0.##E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "Medium Date"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "$00000E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "#0.0#e+#);;$?.0#E+00\%;@", 4, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-51.563), "+00000.###;#,##0,,.000"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.01773, "-#.0#e+# ""u"""): s = s & Ap(v)
    C127 = s
End Function
Private Function C128() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("abc", "Currency", 3, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(23, "mm.d.q am/pm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(89), " ?.####E+0);#0.0 ""u"";""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(49060000000000, "$#0.0#\%", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "mmmm,ddddd-dddd w", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("+4", "ddd/s"): s = s & Ap(v)
    C128 = s
End Function
Private Function C129() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(CCur((-16)), "ss,d", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-340202.946), "(#,##0,,E-0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(Empty, "+? kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/30/1899#, "\xE+00)", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("abc", ";; ?.###E+00;@", 2, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-90639.848), "#0E+0 ""u"""): s = s & Ap(v)
    C129 = s
End Function
Private Function C130() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("x", "0,.###E+00", 7, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-75.6), "$# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "Medium Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("-3", "ddd.dd smmm:nn", 2, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(" ", "nnddddd:\Q", 6, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-33), "$.####E-0\%;;-#0E-0%;@"): s = s & Ap(v)
    C130 = s
End Function
Private Function C131() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(198500000, "(##0E-0 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-55), """<""@"">""", 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(6.930999999999999E-06, ">&", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(20035, "yAM/PM/a/p-ww"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(4.186E-08, "Long Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.68, "(.00e+#%;;$0.0#E+00;@", 7): s = s & Ap(v)
    C131 = s
End Function
Private Function C132() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-423827.518), """ab""?.0#e+#;; 00000.0#;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(632.42, "0, ""u"";;$.0#);@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1E+400", "A/P-AMPM", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "am/pm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", " #,##0.00 kg", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-8)), "-#,#.0000E+00\%", 5): s = s & Ap(v)
    C132 = s
End Function
Private Function C133() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("1/2/2000", "#,##0,,.#E-0;;$##0.####E+0 ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", " 0,.00", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-990.96), "00000.##;#,#.0#E-0 ""u"";""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(1E+300, "Yes/No"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "-0,.### ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-434901.384), "#E+00%"): s = s & Ap(v)
    C133 = s
End Function
Private Function C134() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(9270, "$##0.0#e+#)", 6, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-4.2), "##0.##E-0%", 7, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.125, "#0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(0.47289), "-000 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "A/P qww", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-94.7), "0,.####"): s = s & Ap(v)
    C134 = s
End Function
Private Function C135() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-2.5), "c", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "mmmmm:qttttt-yy,d"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("  7  ", "$#.###E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-19597)), "+#0E-0 ""u""", 5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-442430.423), "m dddddn,ww dd", 7, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", " 0.0#;\x.0#\%; "): s = s & Ap(v)
    C135 = s
End Function
Private Function C136() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur((-10029)), """ab""#,##0,,E-0\%;(##0E+0 kg;0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.9822, " #0.0#;;\x#0.0#;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.031, "-#.##E+00;E+00); "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, " 0.###;-000.##;0 ", 0, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(32767.5, "@@@@@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(DateSerial(1949, 7, 22), "ssss yyyy", 3, 2): s = s & Ap(v)
    C136 = s
End Function
Private Function C137() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("0x10", "\x ""u"";\x#0.000e+# ""u"";0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-2), "-0.0#E-0;;+.0#E+0\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-35736), """T""mmmm/mmmmm,dd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-85.5), """<""@"">"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.847, "+.0E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(103.37, "mm"): s = s & Ap(v)
    C137 = s
End Function
Private Function C138() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-327729.4), "q/ttttt,\Q/ss", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("2147483648", "$#.000E+00;#,##0,,.000E-0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("2.5", """ab""##0.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/100#, "m.AM/PM/w.hh", 1, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.2, "?.0000"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "?.#e+#"): s = s & Ap(v)
    C138 = s
End Function
Private Function C139() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$((-187044.971), "$0.####E+0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-120127.131)), " ?.0#E-0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-2.5), "Currency"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur((-50.8)), "!&&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("x", "m-yy:q"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-12.7186), "-#,##0e+#;""ab"".#E-0"): s = s & Ap(v)
    C139 = s
End Function
Private Function C140() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("", """ab""#,# ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-2.7), "n a/pq/ddd", 5): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "0, ""u"";; 00000.###e+# ""u"";@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(4.4, "\x0E-0;""ab""0.# ""u""", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-80.7), "$000 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CCur(4.809000000000001E-05), "\x#,#.###%;(0,.0# ""u""", 4, 0): s = s & Ap(v)
    C140 = s
End Function
Private Function C141() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("x", "$00000.# ""u"";(000E-0;- "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(70693.393), "standard"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(21990, "+#,##0,,.0#E-0%", 2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(1000000000000000, "00000.0#e+#; #,##0.0#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/1899#, "0.0#e+#;""ab""#,##0,,.0;""zero"" ", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-752.87), "\x#,##0,,.0#E+00;;-#.0e+#);@"): s = s & Ap(v)
    C141 = s
End Function
Private Function C142() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(CCur(152347.162), " .0#E-0\%;\x0.0#;0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(922337203685477.6), "nnqmmm", 2, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-22.15), "#,#.## ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("  7  ", """ab""0.00", 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "+#,##0,,.000E+00", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(8.8, "000.### kg"): s = s & Ap(v)
    C142 = s
End Function
Private Function C143() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("$5", "-.000;(0E+0; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "(?e+#\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "Long Time"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-7.5), "#0e+#;;""ab""#0.0# kg;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("abc", "00000.0E+0;;\x0.000E+0\%;@", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-193423.941), "Long Time"): s = s & Ap(v)
    C143 = s
End Function
Private Function C144() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("", "-?.0# ""u""; #%", 0, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "0.0000\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#2/29/2000 12:00:00 PM#, "Percent"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-9)), "-00000E-0\%;""ab""#.0#E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-330.56), "(0.00\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-7.8), " ##0.0#E+0\%;; ?.0000 kg;@", 4): s = s & Ap(v)
    C144 = s
End Function
Private Function C145() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("abc", "#,##0,,.00", 7, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/1899#, "-00000.0# ""u""", 3, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-118), "yyAM/PM/y:q"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-317353.194), "#%;-0,e+# kg; ", 5, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(7.271000000000001E-08, " ##0.####E+0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#12/30/1899#, "#,##0,, kg", 3): s = s & Ap(v)
    C145 = s
End Function
Private Function C146() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(Null, "0.0#E-0\%", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(CDate(46002.253), "0\%;;#.###E+00\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.62993, "-#.####E+00 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("2147483648", "\x?.### kg", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/9999 11:59:59 PM#, "#0E+00", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Null, "0;+##0.0#E-0 ""u""", 7): s = s & Ap(v)
    C146 = s
End Function
Private Function C147() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("abc", "(##0.0#E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-43)), "+?.0#E+00;;#0.000E-0\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(0.780279, "(#,##0.#E+00;$00000.###"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("-3", "s", 3, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(2, """ab""#.0#e+# ""u""", 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1e3", "Percent"): s = s & Ap(v)
    C147 = s
End Function
Private Function C148() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("-3", "#,#.0#E+0\%;.0E+0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.125, "(#,##0,,.##e+# ""u"";;$?.0000\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(3.7), "?E+0;#0.000", 6, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(1E+300), "#,#.0#E-0)", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(565.81, "ddd-mmmmmA/P nn h"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.3, "0.#%;;(0.#;@"): s = s & Ap(v)
    C148 = s
End Function
Private Function C149() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("$5", " 000E-0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(5.9, "0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-62), "dd,ddd,mmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/100#, "am/pm/dddd-yyyy d/dddd"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "+##0E+0 ""u"""): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#12/31/9999 11:59:59 PM#, "ttttt/yyyy,am/pm", 5, 2): s = s & Ap(v)
    C149 = s
End Function
Private Function C150() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format((-31.609), """ab""E+00", 4, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(50830, " #0\%;(#.0000E+0; ", 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.3019, "-#.0#;; 0.#;@", 7, 0): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-2.1637), "(0);""ab""#0.0"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-349.13), "dddd,AMPMn", 3, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-19349), "!&&", 5): s = s & Ap(v)
    C150 = s
End Function
Private Function C151() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$(True, "@@@@@@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(8.002000000000001E-05, "dd", 0, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-427.75), "mmm/A/P:nn", 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(False, "$0E+0);;+000;@", 1, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(DateSerial(1917, 8, 16), "\x0,.00E+0);""ab""#.#e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-52.1), "+#,##0.###%;(00000.0#E-0)"): s = s & Ap(v)
    C151 = s
End Function
Private Function C152() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(Empty, ".00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#1/1/100#, "+.0E+00;#,##0.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "ss:""T""hh yyyy"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "-#,##0.0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-47.07), "00000.000"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-8.3)), "mm.mmmmmam/pm/dddddd:ddddd"): s = s & Ap(v)
    C152 = s
End Function
Private Function C153() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(384600, "General Date"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(408958.467, "(0,E+00 kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1d2", "&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.5415, "%;;00000E+00;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(15732, "\x?.0#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "\x#,##0,,.###E-0"): s = s & Ap(v)
    C153 = s
End Function
Private Function C154() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format("", "h\Q mmmmm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(True, "#0.00E-0)"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.53E-11, "\x#,##0.0#E+00"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1E+400", "$000.###)", 4): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(839.73, "#E+0;(00000);0 "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, " #,#E-0)", 7): s = s & Ap(v)
    C154 = s
End Function
Private Function C155() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(33, "#,##0E+00;;?.0000e+#\%;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(0.5, "00000.##E+00 ""u"";""ab""0.0#E+0)", 7): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1e3", "Currency", 5, 2): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(#1/1/2000#, "#,##0,,.00%;-0,.0#e+#;""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur((-928.77)), "@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "\x##0.0)"): s = s & Ap(v)
    C155 = s
End Function
Private Function C156() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("abc", "$?.0#e+#;\x#0.# ""u"";""zero"" "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format((-170274.865), "##0E+0\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("Hello World", "\x0,.0#e+#", 5, 1): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("32768", """T"".hh/ddd.ddddd-y"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1E+400", "&&&", 6): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(3.4E+38, "+.0000;;(#,##0e+#;@", 0): s = s & Ap(v)
    C156 = s
End Function
Private Function C157() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(2.1, "#,##0e+#"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(Empty, "&"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#1/1/2000#, "A/Pm"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-910.69), "+?.####E+00\%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("", "E-0 ""u"";;""ab""E-0 kg;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CDate(20), "Long Time"): s = s & Ap(v)
    C157 = s
End Function
Private Function C158() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format$("abc", "True/False"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("1,234", "(000.00E-0;;+?.##;@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(454034.296), "000.0E+0;$#,##0,,.###E-0\%; "): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(" 12 ", "#0;-#,# kg"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-951.71), "Short Date"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format(CCur(300000000000), " 0.###E+00 ""u"""): s = s & Ap(v)
    C158 = s
End Function
Private Function C159() As String
    Dim s As String, v As Variant
    On Error Resume Next
    Err.Clear: v = Empty: v = Format(#6:00:00 AM#, "True/False", 2, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("x", "#,##0.0#E-0\%;$#,#%"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$((-50), """ab""000.000E+00 kg;;000.####;@", 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$("Hello World", "<@"): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format("-0", "Standard", 7, 3): s = s & Ap(v)
    Err.Clear: v = Empty: v = Format$(#1/1/100#, """ab""#,##0.000"): s = s & Ap(v)
    C159 = s
End Function
