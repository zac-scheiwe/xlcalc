Attribute VB_Name = "VbaIo"
Option Explicit

Private Function Dp() As String
    Dp = Environ$("TEMP") & "\xlio"
End Function
Private Function Fp(n As String) As String
    Fp = Dp() & "\" & n
End Function
Private Sub Start()
    On Error Resume Next
    Close
    MkDir Dp()
End Sub
Private Sub Gone(ParamArray n() As Variant)
    Dim i As Long
    On Error Resume Next
    For i = LBound(n) To UBound(n)
        Kill Fp(CStr(n(i)))
    Next i
End Sub
Private Sub W(p As String, s As String)
    On Error Resume Next
    Kill p
    On Error GoTo 0
    Open p For Binary As #9
    Put #9, , s
    Close #9
End Sub
Private Function Hx(p As String) As String
    Dim n As Long, i As Long, c As Byte, o As String
    Open p For Binary As #9
    n = LOF(9)
    For i = 1 To n
        Get #9, i, c
        If c = 13 Then
            o = o & "<r>"
        ElseIf c = 10 Then
            o = o & "<n>"
        ElseIf c >= 32 And c < 127 Then
            o = o & Chr$(c)
        Else
            o = o & "<" & Right$("0" & Hex$(c), 2) & ">"
        End If
    Next i
    Close #9
    Hx = o
End Function
Private Function T(v As Variant) As String
    If IsNull(v) Then
        T = "Null"
    ElseIf IsEmpty(v) Then
        T = "Empty"
    Else
        T = TypeName(v) & ":" & CStr(v)
    End If
End Function

Public Function VbaIoRun(ByVal k As Long) As Variant
    Select Case k \ 100
    Case 0: VbaIoRun = MC0(k)
    Case 1: VbaIoRun = MC1(k)
    Case 2: VbaIoRun = MC2(k)
    Case 3: VbaIoRun = MC3(k)
    End Select
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
    Case 50: MC0 = M50()
    Case 51: MC0 = M51()
    Case 52: MC0 = M52()
    Case 53: MC0 = M53()
    Case 54: MC0 = M54()
    Case 55: MC0 = M55()
    Case 56: MC0 = M56()
    Case 57: MC0 = M57()
    Case 58: MC0 = M58()
    Case 59: MC0 = M59()
    Case 60: MC0 = M60()
    Case 61: MC0 = M61()
    Case 62: MC0 = M62()
    Case 63: MC0 = M63()
    Case 64: MC0 = M64()
    Case 65: MC0 = M65()
    Case 66: MC0 = M66()
    Case 67: MC0 = M67()
    Case 68: MC0 = M68()
    Case 69: MC0 = M69()
    Case 70: MC0 = M70()
    Case 71: MC0 = M71()
    Case 72: MC0 = M72()
    Case 73: MC0 = M73()
    Case 74: MC0 = M74()
    Case 75: MC0 = M75()
    Case 76: MC0 = M76()
    Case 77: MC0 = M77()
    Case 78: MC0 = M78()
    Case 79: MC0 = M79()
    Case 80: MC0 = M80()
    Case 81: MC0 = M81()
    Case 82: MC0 = M82()
    Case 83: MC0 = M83()
    Case 84: MC0 = M84()
    Case 85: MC0 = M85()
    Case 86: MC0 = M86()
    Case 87: MC0 = M87()
    Case 88: MC0 = M88()
    Case 89: MC0 = M89()
    Case 90: MC0 = M90()
    Case 91: MC0 = M91()
    Case 92: MC0 = M92()
    Case 93: MC0 = M93()
    Case 94: MC0 = M94()
    Case 95: MC0 = M95()
    Case 96: MC0 = M96()
    Case 97: MC0 = M97()
    Case 98: MC0 = M98()
    Case 99: MC0 = M99()
    End Select
    Exit Function
EH:
    MC0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 100: MC1 = M100()
    Case 101: MC1 = M101()
    Case 102: MC1 = M102()
    Case 103: MC1 = M103()
    Case 104: MC1 = M104()
    Case 105: MC1 = M105()
    Case 106: MC1 = M106()
    Case 107: MC1 = M107()
    Case 108: MC1 = M108()
    Case 109: MC1 = M109()
    Case 110: MC1 = M110()
    Case 111: MC1 = M111()
    Case 112: MC1 = M112()
    Case 113: MC1 = M113()
    Case 114: MC1 = M114()
    Case 115: MC1 = M115()
    Case 116: MC1 = M116()
    Case 117: MC1 = M117()
    Case 118: MC1 = M118()
    Case 119: MC1 = M119()
    Case 120: MC1 = M120()
    Case 121: MC1 = M121()
    Case 122: MC1 = M122()
    Case 123: MC1 = M123()
    Case 124: MC1 = M124()
    Case 125: MC1 = M125()
    Case 126: MC1 = M126()
    Case 127: MC1 = M127()
    Case 128: MC1 = M128()
    Case 129: MC1 = M129()
    Case 130: MC1 = M130()
    Case 131: MC1 = M131()
    Case 132: MC1 = M132()
    Case 133: MC1 = M133()
    Case 134: MC1 = M134()
    Case 135: MC1 = M135()
    Case 136: MC1 = M136()
    Case 137: MC1 = M137()
    Case 138: MC1 = M138()
    Case 139: MC1 = M139()
    Case 140: MC1 = M140()
    Case 141: MC1 = M141()
    Case 142: MC1 = M142()
    Case 143: MC1 = M143()
    Case 144: MC1 = M144()
    Case 145: MC1 = M145()
    Case 146: MC1 = M146()
    Case 147: MC1 = M147()
    Case 148: MC1 = M148()
    Case 149: MC1 = M149()
    Case 150: MC1 = M150()
    Case 151: MC1 = M151()
    Case 152: MC1 = M152()
    Case 153: MC1 = M153()
    Case 154: MC1 = M154()
    Case 155: MC1 = M155()
    Case 156: MC1 = M156()
    Case 157: MC1 = M157()
    Case 158: MC1 = M158()
    Case 159: MC1 = M159()
    Case 160: MC1 = M160()
    Case 161: MC1 = M161()
    Case 162: MC1 = M162()
    Case 163: MC1 = M163()
    Case 164: MC1 = M164()
    Case 165: MC1 = M165()
    Case 166: MC1 = M166()
    Case 167: MC1 = M167()
    Case 168: MC1 = M168()
    Case 169: MC1 = M169()
    Case 170: MC1 = M170()
    Case 171: MC1 = M171()
    Case 172: MC1 = M172()
    Case 173: MC1 = M173()
    Case 174: MC1 = M174()
    Case 175: MC1 = M175()
    Case 176: MC1 = M176()
    Case 177: MC1 = M177()
    Case 178: MC1 = M178()
    Case 179: MC1 = M179()
    Case 180: MC1 = M180()
    Case 181: MC1 = M181()
    Case 182: MC1 = M182()
    Case 183: MC1 = M183()
    Case 184: MC1 = M184()
    Case 185: MC1 = M185()
    Case 186: MC1 = M186()
    Case 187: MC1 = M187()
    Case 188: MC1 = M188()
    Case 189: MC1 = M189()
    Case 190: MC1 = M190()
    Case 191: MC1 = M191()
    Case 192: MC1 = M192()
    Case 193: MC1 = M193()
    Case 194: MC1 = M194()
    Case 195: MC1 = M195()
    Case 196: MC1 = M196()
    Case 197: MC1 = M197()
    Case 198: MC1 = M198()
    Case 199: MC1 = M199()
    End Select
    Exit Function
EH:
    MC1 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC2(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 200: MC2 = M200()
    Case 201: MC2 = M201()
    Case 202: MC2 = M202()
    Case 203: MC2 = M203()
    Case 204: MC2 = M204()
    Case 205: MC2 = M205()
    Case 206: MC2 = M206()
    Case 207: MC2 = M207()
    Case 208: MC2 = M208()
    Case 209: MC2 = M209()
    Case 210: MC2 = M210()
    Case 211: MC2 = M211()
    Case 212: MC2 = M212()
    Case 213: MC2 = M213()
    Case 214: MC2 = M214()
    Case 215: MC2 = M215()
    Case 216: MC2 = M216()
    Case 217: MC2 = M217()
    Case 218: MC2 = M218()
    Case 219: MC2 = M219()
    Case 220: MC2 = M220()
    Case 221: MC2 = M221()
    Case 222: MC2 = M222()
    Case 223: MC2 = M223()
    Case 224: MC2 = M224()
    Case 225: MC2 = M225()
    Case 226: MC2 = M226()
    Case 227: MC2 = M227()
    Case 228: MC2 = M228()
    Case 229: MC2 = M229()
    Case 230: MC2 = M230()
    Case 231: MC2 = M231()
    Case 232: MC2 = M232()
    Case 233: MC2 = M233()
    Case 234: MC2 = M234()
    Case 235: MC2 = M235()
    Case 236: MC2 = M236()
    Case 237: MC2 = M237()
    Case 238: MC2 = M238()
    Case 239: MC2 = M239()
    Case 240: MC2 = M240()
    Case 241: MC2 = M241()
    Case 242: MC2 = M242()
    Case 243: MC2 = M243()
    Case 244: MC2 = M244()
    Case 245: MC2 = M245()
    Case 246: MC2 = M246()
    Case 247: MC2 = M247()
    Case 248: MC2 = M248()
    Case 249: MC2 = M249()
    Case 250: MC2 = M250()
    Case 251: MC2 = M251()
    Case 252: MC2 = M252()
    Case 253: MC2 = M253()
    Case 254: MC2 = M254()
    Case 255: MC2 = M255()
    Case 256: MC2 = M256()
    Case 257: MC2 = M257()
    Case 258: MC2 = M258()
    Case 259: MC2 = M259()
    Case 260: MC2 = M260()
    Case 261: MC2 = M261()
    Case 262: MC2 = M262()
    Case 263: MC2 = M263()
    Case 264: MC2 = M264()
    Case 265: MC2 = M265()
    Case 266: MC2 = M266()
    Case 267: MC2 = M267()
    Case 268: MC2 = M268()
    Case 269: MC2 = M269()
    Case 270: MC2 = M270()
    Case 271: MC2 = M271()
    Case 272: MC2 = M272()
    Case 273: MC2 = M273()
    Case 274: MC2 = M274()
    Case 275: MC2 = M275()
    Case 276: MC2 = M276()
    Case 277: MC2 = M277()
    Case 278: MC2 = M278()
    Case 279: MC2 = M279()
    Case 280: MC2 = M280()
    Case 281: MC2 = M281()
    Case 282: MC2 = M282()
    Case 283: MC2 = M283()
    Case 284: MC2 = M284()
    Case 285: MC2 = M285()
    Case 286: MC2 = M286()
    Case 287: MC2 = M287()
    Case 288: MC2 = M288()
    Case 289: MC2 = M289()
    Case 290: MC2 = M290()
    Case 291: MC2 = M291()
    Case 292: MC2 = M292()
    Case 293: MC2 = M293()
    Case 294: MC2 = M294()
    Case 295: MC2 = M295()
    Case 296: MC2 = M296()
    Case 297: MC2 = M297()
    Case 298: MC2 = M298()
    Case 299: MC2 = M299()
    End Select
    Exit Function
EH:
    MC2 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC3(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 300: MC3 = M300()
    Case 301: MC3 = M301()
    Case 302: MC3 = M302()
    Case 303: MC3 = M303()
    Case 304: MC3 = M304()
    Case 305: MC3 = M305()
    Case 306: MC3 = M306()
    Case 307: MC3 = M307()
    Case 308: MC3 = M308()
    Case 309: MC3 = M309()
    Case 310: MC3 = M310()
    Case 311: MC3 = M311()
    Case 312: MC3 = M312()
    Case 313: MC3 = M313()
    Case 314: MC3 = M314()
    Case 315: MC3 = M315()
    Case 316: MC3 = M316()
    End Select
    Exit Function
EH:
    MC3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M0() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a"; "b"; "c"
    Close #1
    R = Hx(Fp("a"))
    M0 = R
    Exit Function
EH:
    M0 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M1() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a", "b", "c"
    Close #1
    R = Hx(Fp("a"))
    M1 = R
    Exit Function
EH:
    M1 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M2() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "abcdefghijklmn", "x"
    Close #1
    R = Hx(Fp("a"))
    M2 = R
    Exit Function
EH:
    M2 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M3() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "abcdefghijklm", "x"
    Close #1
    R = Hx(Fp("a"))
    M3 = R
    Exit Function
EH:
    M3 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M4() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "abcdefghijklmnop", "x", "y"
    Close #1
    R = Hx(Fp("a"))
    M4 = R
    Exit Function
EH:
    M4 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M5() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, 1; 2; -3; 4.5
    Close #1
    R = Hx(Fp("a"))
    M5 = R
    Exit Function
EH:
    M5 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M6() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, 1, 2, -3, 4.5
    Close #1
    R = Hx(Fp("a"))
    M6 = R
    Exit Function
EH:
    M6 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M7() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, 1E10; 1E-10; 123456789012345678#; 0.1!; 0.1
    Close #1
    R = Hx(Fp("a"))
    M7 = R
    Exit Function
EH:
    M7 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M8() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, True; False; Null; Empty
    Close #1
    R = Hx(Fp("a"))
    M8 = R
    Exit Function
EH:
    M8 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M9() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, #1/2/2000#; #3:04:05 PM#; #1/2/2000 3:04:05 PM#
    Close #1
    R = Hx(Fp("a"))
    M9 = R
    Exit Function
EH:
    M9 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M10() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, CCur(1.5); CDec(2.25); CByte(7); CInt(-8)
    Close #1
    R = Hx(Fp("a"))
    M10 = R
    Exit Function
EH:
    M10 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M11() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, CVErr(2007)
    Close #1
    R = Hx(Fp("a"))
    M11 = R
    Exit Function
EH:
    M11 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M12() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a";
    Print #1, "b"
    Close #1
    R = Hx(Fp("a"))
    M12 = R
    Exit Function
EH:
    M12 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M13() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a",
    Print #1, "b"
    Close #1
    R = Hx(Fp("a"))
    M13 = R
    Exit Function
EH:
    M13 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M14() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, ""
    Print #1, ""
    Close #1
    R = Hx(Fp("a"))
    M14 = R
    Exit Function
EH:
    M14 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M15() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a"; Spc(3); "b"
    Close #1
    R = Hx(Fp("a"))
    M15 = R
    Exit Function
EH:
    M15 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M16() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a"; Tab(6); "b"
    Close #1
    R = Hx(Fp("a"))
    M16 = R
    Exit Function
EH:
    M16 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M17() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "abcdefgh"; Tab(3); "b"
    Close #1
    R = Hx(Fp("a"))
    M17 = R
    Exit Function
EH:
    M17 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M18() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a"; Tab; "b"
    Close #1
    R = Hx(Fp("a"))
    M18 = R
    Exit Function
EH:
    M18 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M19() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a", Tab(20); "b"
    Close #1
    R = Hx(Fp("a"))
    M19 = R
    Exit Function
EH:
    M19 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M20() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, Spc(0); "x"; Spc(-1); "y"
    Close #1
    R = Hx(Fp("a"))
    M20 = R
    Exit Function
EH:
    M20 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M21() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "x" & vbCrLf & "y"
    Close #1
    R = Hx(Fp("a"))
    M21 = R
    Exit Function
EH:
    M21 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M22() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "x" & vbLf & "y"
    Close #1
    R = Hx(Fp("a"))
    M22 = R
    Exit Function
EH:
    M22 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M23() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "e" & Chr$(233) & ChrW(8364) & ChrW(955)
    Close #1
    R = Hx(Fp("a"))
    M23 = R
    Exit Function
EH:
    M23 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M24() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, 1 + 1; "="; 2
    Close #1
    R = Hx(Fp("a"))
    M24 = R
    Exit Function
EH:
    M24 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M25() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, -0.5; .5; 100; 1000000; 1E15; 1E16
    Close #1
    R = Hx(Fp("a"))
    M25 = R
    Exit Function
EH:
    M25 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M26() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, 3 > 2; 3 < 2
    Close #1
    R = Hx(Fp("a"))
    M26 = R
    Exit Function
EH:
    M26 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M27() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Dim v As Variant
    v = 5
    Print #1, v; "x"
    Close #1
    R = Hx(Fp("a"))
    M27 = R
    Exit Function
EH:
    M27 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M28() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, Format(5, "00"); Str(5); CStr(5)
    Close #1
    R = Hx(Fp("a"))
    M28 = R
    Exit Function
EH:
    M28 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M29() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, 1.5; 2
    Print #1, 3
    Close #1
    R = Hx(Fp("a"))
    M29 = R
    Exit Function
EH:
    M29 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M30() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Dim a As Long: a = 3
    Print #1, a, a
    Close #1
    R = Hx(Fp("a"))
    M30 = R
    Exit Function
EH:
    M30 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M31() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, String$(20, "z"); "y"
    Close #1
    R = Hx(Fp("a"))
    M31 = R
    Exit Function
EH:
    M31 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M32() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a"; ; "b"
    Close #1
    R = Hx(Fp("a"))
    M32 = R
    Exit Function
EH:
    M32 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M33() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "a",, "b"
    Close #1
    R = Hx(Fp("a"))
    M33 = R
    Exit Function
EH:
    M33 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M34() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, ,"b"
    Close #1
    R = Hx(Fp("a"))
    M34 = R
    Exit Function
EH:
    M34 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M35() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, ;"b"
    Close #1
    R = Hx(Fp("a"))
    M35 = R
    Exit Function
EH:
    M35 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M36() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, "a", "b"
    Close #1
    R = Hx(Fp("a"))
    M36 = R
    Exit Function
EH:
    M36 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M37() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, 1, 2.5, -3
    Close #1
    R = Hx(Fp("a"))
    M37 = R
    Exit Function
EH:
    M37 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M38() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, "a"; "b"
    Close #1
    R = Hx(Fp("a"))
    M38 = R
    Exit Function
EH:
    M38 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M39() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, True, False
    Close #1
    R = Hx(Fp("a"))
    M39 = R
    Exit Function
EH:
    M39 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M40() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, Null, Empty
    Close #1
    R = Hx(Fp("a"))
    M40 = R
    Exit Function
EH:
    M40 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M41() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, #1/2/2000#, #3:04:05 PM#, #1/2/2000 3:04:05 PM#
    Close #1
    R = Hx(Fp("a"))
    M41 = R
    Exit Function
EH:
    M41 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M42() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, CCur(1.5), CDec(2.25), CByte(7), 0.1!, 0.1
    Close #1
    R = Hx(Fp("a"))
    M42 = R
    Exit Function
EH:
    M42 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M43() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, 1E10, 1E-10, 1E15, 1E16, 123456.789
    Close #1
    R = Hx(Fp("a"))
    M43 = R
    Exit Function
EH:
    M43 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M44() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, CVErr(2007)
    Close #1
    R = Hx(Fp("a"))
    M44 = R
    Exit Function
EH:
    M44 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M45() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, "a";
    Write #1, "b"
    Close #1
    R = Hx(Fp("a"))
    M45 = R
    Exit Function
EH:
    M45 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M46() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, ""
    Write #1, ""
    Close #1
    R = Hx(Fp("a"))
    M46 = R
    Exit Function
EH:
    M46 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M47() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, "say ""hi"""
    Close #1
    R = Hx(Fp("a"))
    M47 = R
    Exit Function
EH:
    M47 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M48() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, "x" & vbCrLf & "y", 1
    Close #1
    R = Hx(Fp("a"))
    M48 = R
    Exit Function
EH:
    M48 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M49() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, "e" & Chr$(233) & ChrW(8364)
    Close #1
    R = Hx(Fp("a"))
    M49 = R
    Exit Function
EH:
    M49 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M50() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, 1 + 1, "="
    Close #1
    R = Hx(Fp("a"))
    M50 = R
    Exit Function
EH:
    M50 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M51() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, , "b"
    Close #1
    R = Hx(Fp("a"))
    M51 = R
    Exit Function
EH:
    M51 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M52() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, "a", , "b"
    Close #1
    R = Hx(Fp("a"))
    M52 = R
    Exit Function
EH:
    M52 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M53() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, Spc(3), "x"
    Close #1
    R = Hx(Fp("a"))
    M53 = R
    Exit Function
EH:
    M53 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M54() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, 5 > 3, "x"
    Close #1
    R = Hx(Fp("a"))
    M54 = R
    Exit Function
EH:
    M54 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M55() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, -0.0, 1000000, 0.5
    Close #1
    R = Hx(Fp("a"))
    M55 = R
    Exit Function
EH:
    M55 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M56() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Dim v As Variant
    v = "s"
    Write #1, v, 1
    Close #1
    R = Hx(Fp("a"))
    M56 = R
    Exit Function
EH:
    M56 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M57() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1,2,3" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long, b As Long, c As Long
    Input #1, a, b, c
    R = a & "|" & b & "|" & c & "|" & EOF(1)
    Close #1
    M57 = R
    Exit Function
EH:
    M57 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M58() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1,2" & vbCrLf & "3" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long, b As Long, c As Long
    Input #1, a, b
    Input #1, c
    R = a & "|" & b & "|" & c & "|" & EOF(1)
    Close #1
    M58 = R
    Exit Function
EH:
    M58 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M59() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1,2,3" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long, b As Long, c As Long
    Input #1, a, b
    Input #1, c
    R = a & "|" & b & "|" & c & "|" & EOF(1)
    Close #1
    M59 = R
    Exit Function
EH:
    M59 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M60() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc,""d,e"",  f  " & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As String, b As String, c As String
    Input #1, a, b, c
    R = "[" & a & "][" & b & "][" & c & "]"
    Close #1
    M60 = R
    Exit Function
EH:
    M60 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M61() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "3.50,abc" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As String, b As String
    Input #1, a, b
    R = "[" & a & "][" & b & "]"
    Close #1
    M61 = R
    Exit Function
EH:
    M61 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M62() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long
    Input #1, a
    R = a
    Close #1
    M62 = R
    Exit Function
EH:
    M62 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M63() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1x" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long
    Input #1, a
    R = a
    Close #1
    M63 = R
    Exit Function
EH:
    M63 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M64() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), ",5" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long, b As Long
    a = 9
    Input #1, a, b
    R = a & "|" & b
    Close #1
    M64 = R
    Exit Function
EH:
    M64 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M65() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "99999" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Integer
    Input #1, a
    R = a
    Close #1
    M65 = R
    Exit Function
EH:
    M65 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M66() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1.5" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long
    Input #1, a
    R = a
    Close #1
    M66 = R
    Exit Function
EH:
    M66 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M67() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "2.5" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long
    Input #1, a
    R = a
    Close #1
    M67 = R
    Exit Function
EH:
    M67 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M68() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "  7  ,  8" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long, b As Long
    Input #1, a, b
    R = a & "|" & b
    Close #1
    M68 = R
    Exit Function
EH:
    M68 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M69() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "5,5.5,100000,1E3,abc,&H10,#TRUE#,#FALSE#" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Variant, i As Long, o As String
    For i = 1 To 8
    Input #1, a
    o = o & T(a) & " "
    Next i
    R = o
    Close #1
    M69 = R
    Exit Function
EH:
    M69 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M70() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "#2000-01-02#,#2000-01-02 03:04:05#,#NULL#,#ERROR 2007#,""" & "," & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Variant, i As Long, o As String
    For i = 1 To 5
    Input #1, a
    o = o & T(a) & " "
    Next i
    R = o
    Close #1
    M70 = R
    Exit Function
EH:
    M70 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M71() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1E40" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Variant
    Input #1, a
    R = T(a)
    Close #1
    M71 = R
    Exit Function
EH:
    M71 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M72() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1E40" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Double
    Input #1, a
    R = a
    Close #1
    M72 = R
    Exit Function
EH:
    M72 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M73() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "0.1" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Single, b As Double
    Input #1, a
    R = a
    Close #1
    M73 = R
    Exit Function
EH:
    M73 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M74() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long
    Input #1, a
    Input #1, a
    R = a
    Close #1
    M74 = R
    Exit Function
EH:
    M74 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M75() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), ""
    Open Fp("a") For Input As #1
    Dim a As Long
    Input #1, a
    R = a
    Close #1
    M75 = R
    Exit Function
EH:
    M75 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M76() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1,2"
    Open Fp("a") For Input As #1
    Dim a As Long, b As Long
    Input #1, a, b
    R = a & "|" & b & "|" & EOF(1)
    Close #1
    M76 = R
    Exit Function
EH:
    M76 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M77() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a" & vbCrLf & vbCrLf & "b"
    Open Fp("a") For Input As #1
    Dim a As String, b As String
    Input #1, a
    Input #1, b
    R = a & "|" & b
    Close #1
    M77 = R
    Exit Function
EH:
    M77 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M78() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), """x" & vbCrLf & "y"" ,z" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As String, b As String
    Input #1, a, b
    R = a & "|" & b
    Close #1
    M78 = R
    Exit Function
EH:
    M78 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M79() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a,b" & vbLf & "c" & vbLf
    Open Fp("a") For Input As #1
    Dim a As String, b As String, c As String
    Input #1, a, b, c
    R = a & b & c
    Close #1
    M79 = R
    Exit Function
EH:
    M79 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M80() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a,b" & vbCr & "c" & vbCr
    Open Fp("a") For Input As #1
    Dim a As String, b As String, c As String
    Input #1, a, b, c
    R = a & b & c
    Close #1
    M80 = R
    Exit Function
EH:
    M80 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M81() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "5" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Boolean
    Input #1, a
    R = a
    Close #1
    M81 = R
    Exit Function
EH:
    M81 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M82() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "#TRUE#" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Boolean
    Input #1, a
    R = a
    Close #1
    M82 = R
    Exit Function
EH:
    M82 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M83() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "#2000-01-02#" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Date
    Input #1, a
    R = a
    Close #1
    M83 = R
    Exit Function
EH:
    M83 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M84() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1/2/2000" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Date
    Input #1, a
    R = a
    Close #1
    M84 = R
    Exit Function
EH:
    M84 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M85() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "12abc" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Variant
    Input #1, a
    R = T(a)
    Close #1
    M85 = R
    Exit Function
EH:
    M85 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M86() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "-5,+6,.5,5." & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Variant, i As Long, o As String
    For i = 1 To 4
    Input #1, a
    o = o & T(a) & " "
    Next i
    R = o
    Close #1
    M86 = R
    Exit Function
EH:
    M86 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M87() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a" & Chr$(233) & Chr$(128) & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As String
    Input #1, a
    R = AscW(Mid$(a, 2, 1)) & "|" & AscW(Mid$(a, 3, 1))
    Close #1
    M87 = R
    Exit Function
EH:
    M87 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M88() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "hello world,x" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As String
    Input #1, a
    R = a
    Close #1
    M88 = R
    Exit Function
EH:
    M88 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M89() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1,2,3" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a As Long, b As Long, c As Long, d As Long
    Input #1, a, b, c, d
    R = "x"
    Close #1
    M89 = R
    Exit Function
EH:
    M89 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M90() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "1,2,3" & vbCrLf
    Open Fp("a") For Input As #1
    Dim a(1 To 2) As Long
    Input #1, a(1), a(2)
    R = a(1) & a(2)
    Close #1
    M90 = R
    Exit Function
EH:
    M90 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M91() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a" & vbCrLf & "b" & vbCrLf
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = s & "|" & EOF(1)
    Line Input #1, s
    R = R & s & "|" & EOF(1)
    Close #1
    M91 = R
    Exit Function
EH:
    M91 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M92() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a" & vbCrLf & "b"
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    Line Input #1, s
    R = s & "|" & EOF(1)
    Close #1
    M92 = R
    Exit Function
EH:
    M92 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M93() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a" & vbLf & "b" & vbLf
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = s
    Line Input #1, s
    R = R & "|" & s
    Close #1
    M93 = R
    Exit Function
EH:
    M93 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M94() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a" & vbCr & "b" & vbCr
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = s
    Line Input #1, s
    R = R & "|" & s
    Close #1
    M94 = R
    Exit Function
EH:
    M94 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M95() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a" & vbCr & vbLf & vbCr & "b"
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = "[" & s & "]"
    Line Input #1, s
    R = R & "[" & s & "]"
    Line Input #1, s
    R = R & "[" & s & "]"
    Close #1
    M95 = R
    Exit Function
EH:
    M95 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M96() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), vbCrLf & vbCrLf & "x"
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = "[" & s & "]"
    Line Input #1, s
    R = R & "[" & s & "]"
    Line Input #1, s
    R = R & "[" & s & "]" & EOF(1)
    Close #1
    M96 = R
    Exit Function
EH:
    M96 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M97() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "a"
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    Line Input #1, s
    Close #1
    M97 = R
    Exit Function
EH:
    M97 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M98() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), ""
    Open Fp("a") For Input As #1
    R = EOF(1) & "|" & LOF(1)
    Dim s As String
    Line Input #1, s
    Close #1
    M98 = R
    Exit Function
EH:
    M98 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M99() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc" & Chr$(26) & "def" & vbCrLf
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = Len(s) & "|" & EOF(1)
    Close #1
    M99 = R
    Exit Function
EH:
    M99 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M100() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc" & Chr$(0) & "def" & vbCrLf
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = Len(s)
    Close #1
    M100 = R
    Exit Function
EH:
    M100 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M101() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "e" & Chr$(233) & Chr$(128) & Chr$(141) & Chr$(255) & vbCrLf
    Open Fp("a") For Input As #1
    Dim s As String, i As Long
    Line Input #1, s
    For i = 1 To Len(s)
    R = R & AscW(Mid$(s, i, 1)) & ","
    Next i
    Close #1
    M101 = R
    Exit Function
EH:
    M101 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M102() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc" & vbCrLf & "de" & vbCrLf
    Open Fp("a") For Input As #1
    R = LOF(1) & "|" & Loc(1) & "|" & Seek(1) & "|" & EOF(1)
    Dim s As String
    Line Input #1, s
    R = R & "|" & Loc(1) & "|" & Seek(1)
    Close #1
    M102 = R
    Exit Function
EH:
    M102 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M103() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc" & vbCrLf & "de" & vbCrLf
    Open Fp("a") For Input As #1
    Dim s As String
    Seek #1, 4
    R = Seek(1)
    Line Input #1, s
    R = R & "[" & s & "]" & Seek(1)
    Close #1
    M103 = R
    Exit Function
EH:
    M103 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M104() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc" & vbCrLf & "de" & vbCrLf
    Open Fp("a") For Input As #1
    Dim s As String
    Seek #1, 5
    Line Input #1, s
    R = "[" & s & "]"
    Seek #1, 1
    Line Input #1, s
    R = R & "[" & s & "]"
    Close #1
    M104 = R
    Exit Function
EH:
    M104 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M105() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc" & vbCrLf
    Open Fp("a") For Input As #1
    Seek #1, 0
    Close #1
    M105 = R
    Exit Function
EH:
    M105 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M106() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc" & vbCrLf
    Open Fp("a") For Input As #1
    Seek #1, 100
    R = Seek(1) & "|" & EOF(1)
    Close #1
    M106 = R
    Exit Function
EH:
    M106 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M107() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), String$(300, "x") & vbCrLf & "y"
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = Len(s) & "|" & Loc(1) & "|" & Seek(1)
    Close #1
    M107 = R
    Exit Function
EH:
    M107 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M108() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), String$(300, "x") & vbCrLf & "y"
    Open Fp("a") For Input As #1
    R = Loc(1)
    Dim s As String
    Line Input #1, s
    Line Input #1, s
    R = R & "|" & Loc(1) & "|" & Seek(1) & "|" & s
    Close #1
    M108 = R
    Exit Function
EH:
    M108 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M109() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc"
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = s & "|" & EOF(1)
    Close #1
    M109 = R
    Exit Function
EH:
    M109 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M110() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc"
    Open Fp("a") For Input As #1
    Dim v As Variant
    Line Input #1, v
    R = T(v)
    Close #1
    M110 = R
    Exit Function
EH:
    M110 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M111() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc"
    Open Fp("a") For Input As #1
    Dim s As String
    Print #1, s
    Close #1
    M111 = R
    Exit Function
EH:
    M111 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M112() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc"
    Open Fp("a") For Input As #1
    Dim s As String
    Write #1, s
    Close #1
    M112 = R
    Exit Function
EH:
    M112 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M113() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc"
    Open Fp("a") For Input As #1
    Dim s As Long
    Get #1, , s
    Close #1
    M113 = R
    Exit Function
EH:
    M113 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M114() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "abc"
    Open Fp("a") For Input As #1
    Dim s As Long
    Put #1, , s
    Close #1
    M114 = R
    Exit Function
EH:
    M114 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M115() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long: x = 258
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M115 = R
    Exit Function
EH:
    M115 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M116() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Integer: x = -2
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M116 = R
    Exit Function
EH:
    M116 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M117() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Byte: x = 200
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M117 = R
    Exit Function
EH:
    M117 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M118() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Single: x = 1.5
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M118 = R
    Exit Function
EH:
    M118 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M119() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Double: x = 1.5
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M119 = R
    Exit Function
EH:
    M119 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M120() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Currency: x = 1.5
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M120 = R
    Exit Function
EH:
    M120 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M121() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Boolean: x = True
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M121 = R
    Exit Function
EH:
    M121 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M122() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Date: x = #1/2/2000#
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M122 = R
    Exit Function
EH:
    M122 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M123() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As String: x = "abc"
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M123 = R
    Exit Function
EH:
    M123 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M124() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As String: x = "a" & Chr$(233) & ChrW(8364)
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M124 = R
    Exit Function
EH:
    M124 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M125() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As String: x = ""
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M125 = R
    Exit Function
EH:
    M125 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M126() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As String * 5: x = "ab"
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M126 = R
    Exit Function
EH:
    M126 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M127() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant: x = 5
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M127 = R
    Exit Function
EH:
    M127 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M128() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant: x = "ab"
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M128 = R
    Exit Function
EH:
    M128 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M129() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant: x = 1.5
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M129 = R
    Exit Function
EH:
    M129 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M130() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant: x = 100000
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M130 = R
    Exit Function
EH:
    M130 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M131() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant: x = True
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M131 = R
    Exit Function
EH:
    M131 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M132() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M132 = R
    Exit Function
EH:
    M132 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M133() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant: x = Null
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M133 = R
    Exit Function
EH:
    M133 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M134() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long: x = 1
    Put #1, 5, x
    Close #1
    R = Hx(Fp("a"))
    M134 = R
    Exit Function
EH:
    M134 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M135() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long: x = 1
    Put #1, , x
    x = 2
    Put #1, , x
    R = Loc(1) & "|" & Seek(1) & "|" & LOF(1) & "|" & EOF(1)
    Close #1
    M135 = R
    Exit Function
EH:
    M135 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M136() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long: x = 1
    Put #1, 3, x
    R = Loc(1) & "|" & Seek(1) & "|" & LOF(1)
    Close #1
    M136 = R
    Exit Function
EH:
    M136 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M137() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long: x = &H01020304
    Put #1, , x
    Seek #1, 1
    Dim b As Byte
    Get #1, , b
    R = b
    Get #1, , b
    R = R & "," & b
    R = R & "|" & Seek(1) & "|" & Loc(1)
    Close #1
    M137 = R
    Exit Function
EH:
    M137 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M138() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long, y As Long
    x = 7
    Put #1, , x
    Get #1, 1, y
    R = y & "|" & Seek(1)
    Close #1
    M138 = R
    Exit Function
EH:
    M138 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M139() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long, y As Long
    y = 99
    Get #1, , y
    R = y
    Close #1
    M139 = R
    Exit Function
EH:
    M139 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M140() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long, y As Long
    x = 7
    Put #1, , x
    y = 99
    Get #1, , y
    R = y & "|" & EOF(1) & "|" & Seek(1)
    Close #1
    M140 = R
    Exit Function
EH:
    M140 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M141() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long, y As Long
    x = 7
    Put #1, , x
    y = 99
    Get #1, 2, y
    R = y & "|" & EOF(1) & "|" & Seek(1)
    Close #1
    M141 = R
    Exit Function
EH:
    M141 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M142() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long, y As Long
    x = 7
    Put #1, , x
    y = 99
    Seek #1, 3
    Get #1, , y
    R = y & "|" & EOF(1) & "|" & Seek(1)
    Close #1
    M142 = R
    Exit Function
EH:
    M142 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M143() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Double, y As Double
    x = 7.5
    Put #1, , x
    Get #1, 1, y
    R = y
    Close #1
    M143 = R
    Exit Function
EH:
    M143 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M144() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As String, y As String
    x = "hello"
    Put #1, , x
    y = Space$(3)
    Get #1, 1, y
    R = y
    Close #1
    M144 = R
    Exit Function
EH:
    M144 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M145() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As String, y As String
    x = "hello"
    Put #1, , x
    y = Space$(8)
    Get #1, 1, y
    R = "[" & y & "]"
    Close #1
    M145 = R
    Exit Function
EH:
    M145 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M146() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As String, y As String
    x = "hello"
    Put #1, , x
    Get #1, 1, y
    R = "[" & y & "]"
    Close #1
    M146 = R
    Exit Function
EH:
    M146 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M147() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As String, y As String * 3
    x = "hello"
    Put #1, , x
    Get #1, 1, y
    R = "[" & y & "]"
    Close #1
    M147 = R
    Exit Function
EH:
    M147 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M148() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant, y As Variant
    x = 5
    Put #1, , x
    x = "ab"
    Put #1, , x
    x = 2.5
    Put #1, , x
    x = CByte(3)
    Put #1, , x
    x = #1/2/2000#
    Put #1, , x
    Seek #1, 1
    Dim i As Long
    For i = 1 To 5
    Get #1, , y
    R = R & T(y) & " "
    Next i
    Close #1
    M148 = R
    Exit Function
EH:
    M148 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M149() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant, y As Variant
    x = Null
    Put #1, , x
    Get #1, 1, y
    R = T(y)
    Close #1
    M149 = R
    Exit Function
EH:
    M149 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M150() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long, y As Long
    x = 5
    Put #1, , x
    Put #1, , x
    R = LOF(1)
    Close #1
    Open Fp("a") For Binary As #1
    R = R & "|" & LOF(1) & "|" & Seek(1) & "|" & Loc(1)
    Close #1
    M150 = R
    Exit Function
EH:
    M150 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M151() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long
    x = 5
    Put #1, , x
    Close #1
    Open Fp("a") For Binary As #1
    Put #1, 2, x
    R = Hx(Fp("a"))
    Close #1
    M151 = R
    Exit Function
EH:
    M151 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M152() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long
    x = 5
    Put #1, , x
    Put #1, 1, x
    Put #1, 0, x
    Close #1
    M152 = R
    Exit Function
EH:
    M152 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M153() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long
    x = 5
    Put #1, -1, x
    Close #1
    M153 = R
    Exit Function
EH:
    M153 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M154() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long
    Get #1, 0, x
    Close #1
    M154 = R
    Exit Function
EH:
    M154 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M155() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long
    Seek #1, 0
    Close #1
    M155 = R
    Exit Function
EH:
    M155 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M156() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Long
    Seek #1, 3
    R = Seek(1) & "|" & LOF(1) & "|" & EOF(1)
    Close #1
    M156 = R
    Exit Function
EH:
    M156 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M157() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim a(1 To 3) As Byte
    a(1) = 65: a(2) = 66: a(3) = 67
    Put #1, , a
    Close #1
    R = Hx(Fp("a"))
    M157 = R
    Exit Function
EH:
    M157 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M158() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim a() As Byte
    a = "xyz"
    Put #1, , a
    Close #1
    R = Hx(Fp("a"))
    M158 = R
    Exit Function
EH:
    M158 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M159() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim a(1 To 3) As Byte, i As Long
    Put #1, , 65
    Put #1, , 66
    Put #1, , 67
    Get #1, 1, a
    R = a(1) & a(2) & a(3)
    Close #1
    M159 = R
    Exit Function
EH:
    M159 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M160() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim a() As Byte
    ReDim a(1 To 2)
    Put #1, , "ab" & "cd"
    Get #1, 1, a
    R = a(1) & "," & a(2)
    Close #1
    M160 = R
    Exit Function
EH:
    M160 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M161() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim a(1 To 2) As Integer
    a(1) = 1: a(2) = 2
    Put #1, , a
    Close #1
    R = Hx(Fp("a"))
    M161 = R
    Exit Function
EH:
    M161 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M162() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Put #1, , "ab"
    Put #1, , 1
    Put #1, , 1.5
    Put #1, , True
    Put #1, , 100000
    Close #1
    R = Hx(Fp("a"))
    M162 = R
    Exit Function
EH:
    M162 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M163() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Currency, y As Currency
    x = 12345.6789
    Put #1, , x
    Get #1, 1, y
    R = y
    Close #1
    M163 = R
    Exit Function
EH:
    M163 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M164() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Single, y As Single
    x = 0.1
    Put #1, , x
    Get #1, 1, y
    R = y
    Close #1
    M164 = R
    Exit Function
EH:
    M164 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M165() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Date, y As Date
    x = #1/2/2000 3:04:05 PM#
    Put #1, , x
    Get #1, 1, y
    R = CDbl(y)
    Close #1
    M165 = R
    Exit Function
EH:
    M165 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M166() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Boolean, y As Boolean
    x = True
    Put #1, , x
    Get #1, 1, y
    R = y
    Close #1
    M166 = R
    Exit Function
EH:
    M166 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M167() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As LongLong
    x = 5
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M167 = R
    Exit Function
EH:
    M167 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M168() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant
    x = CDec(5)
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M168 = R
    Exit Function
EH:
    M168 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M169() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant
    x = CCur(5)
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M169 = R
    Exit Function
EH:
    M169 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M170() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    Dim x As Variant
    x = Array(1, 2)
    Put #1, , x
    Close #1
    R = Hx(Fp("a"))
    M170 = R
    Exit Function
EH:
    M170 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M171() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long: x = 1
    Put #1, 1, x
    x = 2
    Put #1, 3, x
    Close #1
    R = Hx(Fp("a"))
    M171 = R
    Exit Function
EH:
    M171 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M172() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long: x = 1
    Put #1, , x
    Put #1, , x
    R = Loc(1) & "|" & Seek(1) & "|" & LOF(1) & "|" & EOF(1)
    Close #1
    M172 = R
    Exit Function
EH:
    M172 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M173() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long, y As Long: x = 5
    Put #1, 2, x
    Get #1, 2, y
    R = y & "|" & Loc(1) & "|" & Seek(1)
    Close #1
    M173 = R
    Exit Function
EH:
    M173 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M174() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long, y As Long: x = 5
    Put #1, 2, x
    y = 9
    Get #1, 1, y
    R = y
    Close #1
    M174 = R
    Exit Function
EH:
    M174 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M175() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long, y As Long: x = 5
    Put #1, 2, x
    y = 9
    Get #1, 3, y
    R = y
    Close #1
    M175 = R
    Exit Function
EH:
    M175 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M176() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long, y As Long: x = 5
    Put #1, 1, x
    y = 9
    Get #1, 2, y
    R = y & "|" & EOF(1)
    Close #1
    M176 = R
    Exit Function
EH:
    M176 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M177() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As String: x = "abc"
    Put #1, 1, x
    Put #1, 2, "defghij"
    Close #1
    R = Hx(Fp("a"))
    M177 = R
    Exit Function
EH:
    M177 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M178() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As String: x = "abcdefghijkl"
    Put #1, 1, x
    Close #1
    R = Hx(Fp("a"))
    M178 = R
    Exit Function
EH:
    M178 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M179() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As String, y As String: x = "abc"
    Put #1, 1, x
    Get #1, 1, y
    R = "[" & y & "]"
    Close #1
    M179 = R
    Exit Function
EH:
    M179 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M180() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As String * 4, y As String * 4: x = "abc"
    Put #1, 1, x
    Get #1, 1, y
    R = "[" & y & "]"
    Close #1
    M180 = R
    Exit Function
EH:
    M180 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M181() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Double, y As Double: x = 1.5
    Put #1, 1, x
    Get #1, 1, y
    R = y
    Close #1
    M181 = R
    Exit Function
EH:
    M181 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M182() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 4
    Dim x As Double: x = 1.5
    Put #1, 1, x
    Close #1
    R = Hx(Fp("a"))
    M182 = R
    Exit Function
EH:
    M182 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M183() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 2
    Dim x As Long: x = 1
    Put #1, 1, x
    Close #1
    R = Hx(Fp("a"))
    M183 = R
    Exit Function
EH:
    M183 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M184() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Variant: x = 5
    Put #1, 1, x
    Close #1
    R = Hx(Fp("a"))
    M184 = R
    Exit Function
EH:
    M184 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M185() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 10
    Dim x As Variant: x = 5
    Put #1, 1, x
    Close #1
    R = Hx(Fp("a"))
    M185 = R
    Exit Function
EH:
    M185 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M186() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 12
    Dim x As Variant: x = "ab"
    Put #1, 1, x
    Close #1
    R = Hx(Fp("a"))
    M186 = R
    Exit Function
EH:
    M186 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M187() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 12
    Dim x As Variant: x = 1.5
    Put #1, 1, x
    Close #1
    R = Hx(Fp("a"))
    M187 = R
    Exit Function
EH:
    M187 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M188() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long: x = 1
    Put #1, 0, x
    Close #1
    R = Hx(Fp("a"))
    M188 = R
    Exit Function
EH:
    M188 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M189() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long: x = 1
    Put #1, -3, x
    Close #1
    R = Hx(Fp("a"))
    M189 = R
    Exit Function
EH:
    M189 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M190() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long
    Get #1, 1, x
    R = x
    Close #1
    M190 = R
    Exit Function
EH:
    M190 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M191() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long: x = 1
    Put #1, 4, x
    R = LOF(1)
    Close #1
    M191 = R
    Exit Function
EH:
    M191 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M192() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long: x = 1
    Put #1, 4, x
    Seek #1, 2
    R = Seek(1) & "|" & Loc(1)
    Close #1
    M192 = R
    Exit Function
EH:
    M192 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M193() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim x As Long: x = 1
    Put #1, 4, x
    Seek #1, 0
    Close #1
    R = Hx(Fp("a"))
    M193 = R
    Exit Function
EH:
    M193 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M194() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Print #1, "a"
    Close #1
    R = Hx(Fp("a"))
    M194 = R
    Exit Function
EH:
    M194 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M195() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim s As String
    Input #1, s
    Close #1
    R = Hx(Fp("a"))
    M195 = R
    Exit Function
EH:
    M195 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M196() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 8
    Dim s As String
    Line Input #1, s
    Close #1
    R = Hx(Fp("a"))
    M196 = R
    Exit Function
EH:
    M196 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M197() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1
    Dim x As Long: x = 1
    Put #1, 1, x
    Put #1, 2, x
    R = LOF(1)
    Close #1
    R = R & "|" & Hx(Fp("a"))
    M197 = R
    Exit Function
EH:
    M197 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M198() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1
    Dim x As String: x = "abc"
    Put #1, 1, x
    Put #1, 2, x
    R = LOF(1) & "|" & Seek(1)
    Close #1
    M198 = R
    Exit Function
EH:
    M198 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M199() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 0

    M199 = R
    Exit Function
EH:
    M199 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M200() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = -1

    M200 = R
    Exit Function
EH:
    M200 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M201() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 40000
    Close #1
    M201 = R
    Exit Function
EH:
    M201 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M202() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 32767
    R = LOF(1): Close #1
    M202 = R
    Exit Function
EH:
    M202 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M203() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1 Len = 32768
    R = LOF(1): Close #1
    M203 = R
    Exit Function
EH:
    M203 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M204() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1 Len = 4
    Dim x As Long: x = 1
    Put #1, , x
    R = Loc(1)
    Close #1
    M204 = R
    Exit Function
EH:
    M204 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M205() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1 Len = 4
    Print #1, "abc"
    Close #1
    R = Hx(Fp("a"))
    M205 = R
    Exit Function
EH:
    M205 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M206() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "abc"
    Close #1
    Open Fp("a") For Output As #1
    Close #1
    R = FileLen(Fp("a"))
    M206 = R
    Exit Function
EH:
    M206 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M207() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "abc"
    Close #1
    Open Fp("a") For Append As #1
    Print #1, "def"
    Close #1
    R = Hx(Fp("a"))
    M207 = R
    Exit Function
EH:
    M207 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M208() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Append As #1
    R = LOF(1) & "|" & Loc(1) & "|" & Seek(1) & "|" & EOF(1)
    Print #1, "abc"
    R = R & "|" & LOF(1) & "|" & Loc(1) & "|" & Seek(1)
    Close #1
    M208 = R
    Exit Function
EH:
    M208 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M209() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    W Fp("a"), "xyz"
    Open Fp("a") For Append As #1
    R = LOF(1) & "|" & Loc(1) & "|" & Seek(1) & "|" & EOF(1)
    Close #1
    M209 = R
    Exit Function
EH:
    M209 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M210() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    W Fp("a"), "xyz"
    Open Fp("a") For Append As #1
    Dim s As String
    Line Input #1, s
    M210 = R
    Exit Function
EH:
    M210 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M211() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    W Fp("a"), "xyz"
    Open Fp("a") For Output As #1
    R = LOF(1) & "|" & Loc(1) & "|" & Seek(1) & "|" & EOF(1)
    Print #1, "ab"
    R = R & "|" & LOF(1) & "|" & Loc(1) & "|" & Seek(1) & "|" & EOF(1)
    Close #1
    M211 = R
    Exit Function
EH:
    M211 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M212() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Input As #1
    M212 = R
    Exit Function
EH:
    M212 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M213() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Binary As #1
    R = Dir(Fp("a")) & "|" & LOF(1)
    Close #1
    M213 = R
    Exit Function
EH:
    M213 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M214() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Random As #1
    R = Dir(Fp("a")) & "|" & LOF(1)
    Close #1
    M214 = R
    Exit Function
EH:
    M214 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M215() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Open Fp("a") For Output As #2
    M215 = R
    Exit Function
EH:
    M215 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M216() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Open Fp("a") For Input As #2
    M216 = R
    Exit Function
EH:
    M216 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M217() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Open Fp("b") For Output As #1
    M217 = R
    Exit Function
EH:
    M217 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M218() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Open Fp("a") For Input As #1
    Open Fp("a") For Input As #2
    R = "ok"
    Close
    M218 = R
    Exit Function
EH:
    M218 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M219() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Open Fp("a") For Input Shared As #1
    Open Fp("a") For Input Shared As #2
    R = "ok"
    Close
    M219 = R
    Exit Function
EH:
    M219 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M220() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Open Fp("a") For Input Lock Write As #1
    R = "ok"
    Close
    M220 = R
    Exit Function
EH:
    M220 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M221() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Open Fp("a") For Input Access Read As #1
    R = "ok"
    Close
    M221 = R
    Exit Function
EH:
    M221 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M222() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Open Fp("a") For Binary Access Read As #1
    Dim x As Byte
    Get #1, , x
    R = x
    Put #1, , x
    M222 = R
    Exit Function
EH:
    M222 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M223() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Open Fp("a") For Binary Access Write As #1
    Dim x As Byte
    Put #1, , x
    R = "ok"
    Get #1, 1, x
    M223 = R
    Exit Function
EH:
    M223 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M224() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Open Fp("a") For Binary Access Read Write As #1
    R = "ok"
    Close
    M224 = R
    Exit Function
EH:
    M224 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M225() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As 1
    Close 1
    R = "ok"
    M225 = R
    Exit Function
EH:
    M225 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M226() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #0
    M226 = R
    Exit Function
EH:
    M226 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M227() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #256
    M227 = R
    Exit Function
EH:
    M227 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M228() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #255
    R = FreeFile
    Close
    M228 = R
    Exit Function
EH:
    M228 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M229() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #-1
    M229 = R
    Exit Function
EH:
    M229 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M230() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Dim n As Long: n = 3
    Open Fp("a") For Output As n
    R = FreeFile
    Close n
    M230 = R
    Exit Function
EH:
    M230 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M231() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1.5
    R = FreeFile
    Close
    M231 = R
    Exit Function
EH:
    M231 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M232() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #"1"
    R = FreeFile
    Close
    M232 = R
    Exit Function
EH:
    M232 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M233() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Open "" For Output As #1
    M233 = R
    Exit Function
EH:
    M233 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M234() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Open Fp("no\dir\a") For Output As #1
    M234 = R
    Exit Function
EH:
    M234 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M235() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Open Dp() For Input As #1
    M235 = R
    Exit Function
EH:
    M235 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M236() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Open Dp() For Output As #1
    M236 = R
    Exit Function
EH:
    M236 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M237() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Open Dp() For Binary As #1
    M237 = R
    Exit Function
EH:
    M237 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M238() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Open Fp("a?b") For Output As #1
    M238 = R
    Exit Function
EH:
    M238 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M239() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Open Fp("a*b") For Output As #1
    M239 = R
    Exit Function
EH:
    M239 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M240() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Open Fp("a" & Chr$(1)) For Output As #1
    M240 = R
    Exit Function
EH:
    M240 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M241() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Close #2
    Close #1, #1
    R = "ok"
    M241 = R
    Exit Function
EH:
    M241 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M242() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    Open Fp("a") For Output As #1
    Open Fp("b") For Output As #2
    Close #1, #2
    R = FreeFile
    M242 = R
    Exit Function
EH:
    M242 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M243() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    Open Fp("a") For Output As #1
    Open Fp("b") For Output As #2
    Close
    R = FreeFile
    M243 = R
    Exit Function
EH:
    M243 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M244() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    Open Fp("a") For Output As #1
    Open Fp("b") For Output As #2
    Reset
    R = FreeFile
    M244 = R
    Exit Function
EH:
    M244 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M245() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    Open Fp("a") For Output As #3
    Open Fp("b") For Output As #1
    R = FreeFile
    Close #1
    R = R & "|" & FreeFile
    Close
    M245 = R
    Exit Function
EH:
    M245 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M246() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = FreeFile(0) & "|" & FreeFile(1)
    M246 = R
    Exit Function
EH:
    M246 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M247() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = FreeFile(2)
    M247 = R
    Exit Function
EH:
    M247 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M248() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #200
    R = FreeFile(1)
    Close
    M248 = R
    Exit Function
EH:
    M248 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M249() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "x"
    R = FileLen(Fp("a"))
    Close #1
    R = R & "|" & FileLen(Fp("a"))
    M249 = R
    Exit Function
EH:
    M249 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M250() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    R = LOF(2)
    M250 = R
    Exit Function
EH:
    M250 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M251() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = EOF(1)
    M251 = R
    Exit Function
EH:
    M251 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M252() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = Loc(1)
    M252 = R
    Exit Function
EH:
    M252 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M253() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = Seek(1)
    M253 = R
    Exit Function
EH:
    M253 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M254() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = LOF(0)
    M254 = R
    Exit Function
EH:
    M254 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M255() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = LOF(300)
    M255 = R
    Exit Function
EH:
    M255 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M256() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Seek #1, 1
    M256 = R
    Exit Function
EH:
    M256 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M257() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Print #1, "x"
    M257 = R
    Exit Function
EH:
    M257 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M258() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Dim s As String
    Input #1, s
    M258 = R
    Exit Function
EH:
    M258 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M259() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Dim s As String
    Line Input #1, s
    M259 = R
    Exit Function
EH:
    M259 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M260() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Dim x As Long
    Get #1, , x
    M260 = R
    Exit Function
EH:
    M260 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M261() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Dim x As Long
    Put #1, , x
    M261 = R
    Exit Function
EH:
    M261 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M262() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Write #1, "x"
    M262 = R
    Exit Function
EH:
    M262 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M263() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Close #1
    R = "ok"
    M263 = R
    Exit Function
EH:
    M263 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M264() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    R = EOF(1)
    Close
    M264 = R
    Exit Function
EH:
    M264 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M265() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Dim s As String
    Line Input #1, s
    M265 = R
    Exit Function
EH:
    M265 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M266() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Dim x As Long
    Get #1, , x
    M266 = R
    Exit Function
EH:
    M266 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M267() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Kill Fp("a")
    R = Dir(Fp("a")) & "|" & FileLen(Fp("a"))
    M267 = R
    Exit Function
EH:
    M267 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M268() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Kill Fp("a")
    M268 = R
    Exit Function
EH:
    M268 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M269() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Open Fp("a") For Input As #1
    Kill Fp("a")
    M269 = R
    Exit Function
EH:
    M269 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M270() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Kill ""
    M270 = R
    Exit Function
EH:
    M270 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M271() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Kill Fp("no\x")
    M271 = R
    Exit Function
EH:
    M271 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M272() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    W Fp("a"), "x"
    Name Fp("a") As Fp("b")
    R = Dir(Fp("a")) & "|" & Dir(Fp("b"))
    M272 = R
    Exit Function
EH:
    M272 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M273() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    Name Fp("a") As Fp("b")
    M273 = R
    Exit Function
EH:
    M273 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M274() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    W Fp("a"), "x"
    W Fp("b"), "y"
    Name Fp("a") As Fp("b")
    M274 = R
    Exit Function
EH:
    M274 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M275() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    W Fp("a"), "x"
    Open Fp("a") For Input As #1
    Name Fp("a") As Fp("b")
    M275 = R
    Exit Function
EH:
    M275 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M276() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Name Fp("a") As Fp("a")
    M276 = R
    Exit Function
EH:
    M276 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M277() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    Name Fp("a") As Fp("no\b")
    M277 = R
    Exit Function
EH:
    M277 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M278() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    W Fp("a"), "x"
    FileCopy Fp("a"), Fp("b")
    R = FileLen(Fp("b"))
    M278 = R
    Exit Function
EH:
    M278 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M279() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    FileCopy Fp("a"), Fp("b")
    M279 = R
    Exit Function
EH:
    M279 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M280() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    W Fp("a"), "x"
    W Fp("b"), "yy"
    FileCopy Fp("a"), Fp("b")
    R = FileLen(Fp("b"))
    M280 = R
    Exit Function
EH:
    M280 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M281() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    W Fp("a"), "x"
    Open Fp("a") For Input As #1
    FileCopy Fp("a"), Fp("b")
    R = "ok"
    Close
    M281 = R
    Exit Function
EH:
    M281 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M282() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b"
    W Fp("a"), "x"
    W Fp("b"), "yy"
    Open Fp("b") For Input As #1
    FileCopy Fp("a"), Fp("b")
    M282 = R
    Exit Function
EH:
    M282 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M283() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "xyz"
    R = FileLen(Fp("a"))
    M283 = R
    Exit Function
EH:
    M283 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M284() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    R = FileLen(Fp("a"))
    M284 = R
    Exit Function
EH:
    M284 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M285() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = FileLen(Dp())
    M285 = R
    Exit Function
EH:
    M285 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M286() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = FileLen("")
    M286 = R
    Exit Function
EH:
    M286 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M287() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    On Error Resume Next
    RmDir Dp() & "\sub"
    On Error GoTo EH
    MkDir Dp() & "\sub"
    R = Dir(Dp() & "\sub", vbDirectory)
    RmDir Dp() & "\sub"
    R = R & "|" & Dir(Dp() & "\sub", vbDirectory)
    M287 = R
    Exit Function
EH:
    M287 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M288() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    MkDir Dp()
    M288 = R
    Exit Function
EH:
    M288 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M289() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    MkDir Dp() & "\n1\n2"
    M289 = R
    Exit Function
EH:
    M289 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M290() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    MkDir ""
    M290 = R
    Exit Function
EH:
    M290 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M291() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    RmDir Dp() & "\nodir"
    M291 = R
    Exit Function
EH:
    M291 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M292() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    RmDir Fp("a")
    M292 = R
    Exit Function
EH:
    M292 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M293() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    On Error Resume Next
    RmDir Dp() & "\sub"
    On Error GoTo EH
    MkDir Dp() & "\sub"
    W Dp() & "\sub\f", "x"
    RmDir Dp() & "\sub"
    M293 = R
    Exit Function
EH:
    M293 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M294() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    R = Dir(Fp("a")) & "|" & Dir(Fp("b")) & "|" & Dir(Fp("a"))
    M294 = R
    Exit Function
EH:
    M294 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M295() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a", "b", "c"
    W Fp("a"), "x"
    W Fp("b"), "x"
    W Fp("c"), "x"
    Dim f As String
    f = Dir(Dp() & "\?")
    R = f
    f = Dir
    R = R & "," & f
    f = Dir
    R = R & "," & f
    f = Dir
    R = R & ",[" & f & "]"
    Gone "a", "b", "c"
    M295 = R
    Exit Function
EH:
    M295 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M296() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    R = Dir(Dp() & "\*.zzz") & "|"
    M296 = R
    Exit Function
EH:
    M296 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M297() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = Dir("")
    M297 = R
    Exit Function
EH:
    M297 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M298() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = Dir
    M298 = R
    Exit Function
EH:
    M298 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M299() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Dim f As String
    f = Dir(Dp())
    R = "[" & f & "]"
    M299 = R
    Exit Function
EH:
    M299 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M300() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    R = GetAttr(Fp("a")) & "|" & GetAttr(Dp())
    M300 = R
    Exit Function
EH:
    M300 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M301() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    R = GetAttr(Fp("a"))
    M301 = R
    Exit Function
EH:
    M301 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M302() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    SetAttr Fp("a"), vbReadOnly
    R = GetAttr(Fp("a"))
    On Error Resume Next
    Kill Fp("a")
    R = R & "|" & Err.Number
    On Error GoTo EH
    SetAttr Fp("a"), vbNormal
    Kill Fp("a")
    M302 = R
    Exit Function
EH:
    M302 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M303() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    W Fp("a"), "x"
    SetAttr Fp("a"), vbReadOnly
    On Error Resume Next
    Open Fp("a") For Output As #1
    R = Err.Number
    Close
    Err.Clear
    Open Fp("a") For Input As #1
    R = R & "|" & Err.Number
    Close
    SetAttr Fp("a"), vbNormal
    Kill Fp("a")
    M303 = R
    Exit Function
EH:
    M303 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M304() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = CurDir$ = CurDir
    M304 = R
    Exit Function
EH:
    M304 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M305() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = Len(Environ$("TEMP")) > 0
    M305 = R
    Exit Function
EH:
    M305 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M306() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    R = Environ$("XLIO_NOPE_X") & "|" & Environ("XLIO_NOPE_X")
    M306 = R
    Exit Function
EH:
    M306 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M307() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, "a", 1, True, #1/2/2000#
    Print #1, "x"; 2
    Close #1
    Open Fp("a") For Input As #1
    Dim a As Variant, b As Variant, c As Variant, d As Variant, s As String
    Input #1, a, b, c, d
    R = T(a) & T(b) & T(c) & T(d)
    Line Input #1, s
    R = R & "[" & s & "]"
    Close #1
    M307 = R
    Exit Function
EH:
    M307 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M308() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Dim i As Long
    For i = 1 To 5
    Write #1, i, "n" & i
    Next i
    Close #1
    Open Fp("a") For Input As #1
    Dim n As Long, s As String
    Do While Not EOF(1)
    Input #1, n, s
    R = R & n & s & ";"
    Loop
    Close #1
    M308 = R
    Exit Function
EH:
    M308 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M309() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, "l1"
    Print #1, "l2"
    Close #1
    Open Fp("a") For Input As #1
    Dim s As String
    Do Until EOF(1)
    Line Input #1, s
    R = R & s & ";"
    Loop
    Close #1
    R = R & FileLen(Fp("a"))
    M309 = R
    Exit Function
EH:
    M309 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M310() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, 1.5; 1.5!; 1E20
    Close #1
    Open Fp("a") For Input As #1
    Dim s As String
    Line Input #1, s
    R = s
    Close #1
    M310 = R
    Exit Function
EH:
    M310 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M311() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Print #1, 0.1 + 0.2; 1 / 3; 2 ^ 63; -2 ^ 31
    Close #1
    R = Hx(Fp("a"))
    M311 = R
    Exit Function
EH:
    M311 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M312() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Write #1, 0.1 + 0.2, 1 / 3, 2 ^ 63, CSng(1 / 3), 1E+300
    Close #1
    R = Hx(Fp("a"))
    M312 = R
    Exit Function
EH:
    M312 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M313() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    On Error Resume Next
    Print #1, 1 / 0
    R = Err.Number
    Close #1
    R = R & "|" & FileLen(Fp("a"))
    M313 = R
    Exit Function
EH:
    M313 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M314() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    On Error Resume Next
    Print #1, "a"; 1 / 0; "b"
    Close #1
    R = Hx(Fp("a"))
    M314 = R
    Exit Function
EH:
    M314 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M315() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    Dim o As Object
    On Error Resume Next
    Print #1, o
    R = Err.Number
    Close #1
    M315 = R
    Exit Function
EH:
    M315 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function

Private Function M316() As Variant
    On Error GoTo EH
    Dim R As Variant
    Start
    Gone "a"
    Open Fp("a") For Output As #1
    On Error Resume Next
    Print #1, Array(1, 2)
    R = Err.Number
    Close #1
    M316 = R
    Exit Function
EH:
    M316 = "#ERR " & Err.Number & " " & Err.Description
    Close
End Function
