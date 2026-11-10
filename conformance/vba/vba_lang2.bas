Attribute VB_Name = "VbaLang2"
Option Explicit

Public L2Log As String
Private Function fD(x As Variant) As Double
    fD = x
End Function
Private Function fS(x As Variant) As Single
    fS = x
End Function
Private Function fL(x As Variant) As Long
    fL = x
End Function
Private Function Z() As Long
    Z = 0
End Function
Private Function Fa(ParamArray p() As Variant) As Variant
    Fa = p
End Function
Private Sub Fill(ByRef a() As Long)
    Dim i As Long
    For i = LBound(a) To UBound(a)
        a(i) = i * 10
    Next i
End Sub
Private Sub Grow(ByRef a As Variant)
    ReDim Preserve a(LBound(a) To UBound(a) + 1)
End Sub
Private Function MkArr() As Long()
    Dim a(1 To 3) As Long
    a(1) = 1: a(2) = 2: a(3) = 3
    MkArr = a
End Function
Private Function Dump(v As Variant) As String
    Dim s As String, i As Long
    If IsArray(v) Then
        For i = LBound(v) To UBound(v)
            s = s & Dump(v(i)) & ","
        Next i
        Dump = "[" & s & "]"
    ElseIf IsNull(v) Then
        Dump = "Null"
    ElseIf IsEmpty(v) Then
        Dump = "Empty"
    ElseIf IsObject(v) Then
        Dump = "<" & TypeName(v) & ">"
    Else
        Dump = TypeName(v) & ":" & CStr(v)
    End If
End Function

Private Sub Scope1()
    Dim o As New LangCls
    o.Tag = "s"
End Sub

Public Function LangTwo(ByVal k As Long) As Variant
    Select Case k \ 100
    Case 0: LangTwo = MC0(k)
    Case 1: LangTwo = MC1(k)
    Case 2: LangTwo = MC2(k)
    Case 3: LangTwo = MC3(k)
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
    End Select
    Exit Function
EH:
    MC3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M0() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    L2Log = ""
    o.N = 5
    R = o.N & "|" & L2Log
    M0 = R
    Exit Function
EH:
    M0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As LangCls
    L2Log = ""
    Set o = New LangCls
    Set o = Nothing
    R = L2Log
    M1 = R
    Exit Function
EH:
    M1 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As LangCls
    L2Log = ""
    Set o = New LangCls
    o.Tag = "a"
    Set o = New LangCls
    o.Tag = "b"
    Set o = Nothing
    R = L2Log
    M2 = R
    Exit Function
EH:
    M2 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    L2Log = ""
    o.Tag = "x"
    Set o = Nothing
    o.Tag = "y"
    Set o = Nothing
    R = L2Log
    M3 = R
    Exit Function
EH:
    M3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    Dim p As LangCls
    L2Log = ""
    Set p = o
    Set o = Nothing
    R = L2Log & "|" & p.N
    M4 = R
    Exit Function
EH:
    M4 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M5() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As LangCls
    R = (o Is Nothing) & "|" & TypeName(o)
    Set o = New LangCls
    R = R & "|" & (o Is Nothing) & "|" & TypeName(o)
    M5 = R
    Exit Function
EH:
    M5 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M6() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As LangCls
    R = o.N
    M6 = R
    Exit Function
EH:
    M6 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M7() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object
    R = o.N
    M7 = R
    Exit Function
EH:
    M7 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M8() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object
    Set o = New LangCls
    R = o.Nope
    M8 = R
    Exit Function
EH:
    M8 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M9() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object
    Set o = New LangCls
    o.Nope = 1
    M9 = R
    Exit Function
EH:
    M9 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M10() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object
    Set o = New LangCls
    o.RO = 5
    M10 = R
    Exit Function
EH:
    M10 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M11() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    o.V = 5
    R = TypeName(o.V) & o.V
    o.V = "s"
    R = R & TypeName(o.V) & o.V
    o.V = Empty
    R = R & TypeName(o.V)
    o.V = Null
    R = R & TypeName(o.V)
    M11 = R
    Exit Function
EH:
    M11 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M12() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    Dim q As New LangCls
    q.Tag = "q"
    Set o.V = q
    R = TypeName(o.V) & o.V.Tag & IsObject(o.V)
    o.V = 7
    R = R & TypeName(o.V) & IsObject(o.V)
    M12 = R
    Exit Function
EH:
    M12 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M13() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    o.V = Array(1, 2, 3)
    R = TypeName(o.V) & UBound(o.V)
    M13 = R
    Exit Function
EH:
    M13 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M14() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    Dim q As New LangCls
    q.N = 77
    o.V = q
    R = TypeName(o.V)
    M14 = R
    Exit Function
EH:
    M14 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M15() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    L2Log = ""
    o.Item(3) = "z"
    R = o.Item(4) & L2Log
    M15 = R
    Exit Function
EH:
    M15 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M16() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = o.Item(1) & o.Item(2) & o.Twice(4) & o.Twice(fL(5))
    M16 = R
    Exit Function
EH:
    M16 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M17() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    o.Bump
    o.Bump 5
    o.Bump by:=10
    Call o.Bump(100)
    R = o.N & "|" & o.Counter
    M17 = R
    Exit Function
EH:
    M17 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M18() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    Dim x As Long
    x = 1
    o.ByRefSet x
    R = x
    o.ByRefSet (x)
    R = R & "|" & x
    o.ByRefSet o.Counter
    R = R & "|" & o.Counter
    M18 = R
    Exit Function
EH:
    M18 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M19() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = o.Count2() & o.Count2()
    Dim p As New LangCls
    R = R & p.Count2()
    M19 = R
    Exit Function
EH:
    M19 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M20() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = (o.Self Is o) & "|" & o.Self.Self.Twice(2)
    M20 = R
    Exit Function
EH:
    M20 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M21() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    Dim p As New LangCls
    R = (o Is p) & "|" & (o Is o)
    M21 = R
    Exit Function
EH:
    M21 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M22() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    Dim v As Variant
    Set v = o
    R = TypeName(v) & VarType(v) & IsObject(v)
    v.Tag = "t"
    R = R & o.Tag
    M22 = R
    Exit Function
EH:
    M22 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M23() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    o.Tag = "k"
    Dim c As New Collection
    c.Add o
    c.Add o, "key"
    R = c.Count & c("key").Tag & c(1).Tag & (c(1) Is c(2))
    M23 = R
    Exit Function
EH:
    M23 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M24() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = VarType(o) & "|" & TypeName(o) & "|" & (VarType(o) = vbObject)
    M24 = R
    Exit Function
EH:
    M24 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M25() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As LangCls
    Set o = New LangCls
    Dim p As Object
    Set p = o
    R = TypeName(p) & (TypeOf p Is LangCls) & (TypeOf o Is LangCls) & (TypeOf p Is Collection)
    M25 = R
    Exit Function
EH:
    M25 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M26() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = o.N
    o.N = o.N + 1
    o.N = o.N * 2
    R = R & "|" & o.N
    M26 = R
    Exit Function
EH:
    M26 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M27() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    With o
        .Tag = "w"
        .N = 3
        R = .Tag & .N & .Twice(2)
    End With
    M27 = R
    Exit Function
EH:
    M27 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M28() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = o.Item("5")
    R = R & o.Item(2.5)
    R = R & o.Item(True)
    M28 = R
    Exit Function
EH:
    M28 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M29() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As LangSink
    Dim o As LangCls
    Set s = New LangSink
    Set o = New LangCls
    L2Log = ""
    Set s.Src = o
    o.N = 5
    R = L2Log & "|" & o.N
    M29 = R
    Exit Function
EH:
    M29 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M30() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As LangSink
    Dim o As LangCls
    Set s = New LangSink
    Set o = New LangCls
    L2Log = ""
    Set s.Src = o
    s.Veto = True
    o.N = 5
    R = L2Log & "|" & o.N
    M30 = R
    Exit Function
EH:
    M30 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M31() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As LangSink
    Dim o As LangCls
    Set s = New LangSink
    Set o = New LangCls
    L2Log = ""
    Set s.Src = o
    o.Fire
    Set s.Src = Nothing
    o.Fire
    o.N = 9
    R = L2Log
    M31 = R
    Exit Function
EH:
    M31 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M32() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As LangSink, t As LangSink
    Dim o As LangCls
    Set s = New LangSink
    Set t = New LangSink
    Set o = New LangCls
    L2Log = ""
    Set s.Src = o
    Set t.Src = o
    o.Fire
    R = L2Log
    M32 = R
    Exit Function
EH:
    M32 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M33() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As LangSink
    Dim o As LangCls, o2 As LangCls
    Set s = New LangSink
    Set o = New LangCls
    Set o2 = New LangCls
    L2Log = ""
    Set s.Src = o
    Set s.Src = o2
    o.Fire
    o2.Fire
    R = L2Log
    M33 = R
    Exit Function
EH:
    M33 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M34() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As LangSink
    Dim o As LangCls
    Set s = New LangSink
    Set o = New LangCls
    L2Log = ""
    Set s.Src = o
    Set s = Nothing
    o.Fire
    R = L2Log
    M34 = R
    Exit Function
EH:
    M34 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M35() As Variant
    On Error GoTo EH
    Dim R As Variant
    L2Log = ""
    Dim i As Long
    For i = 1 To 3
        Dim o As New LangCls
        o.Tag = CStr(i)
    Next i
    R = L2Log
    M35 = R
    Exit Function
EH:
    M35 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M36() As Variant
    On Error GoTo EH
    Dim R As Variant
    L2Log = ""
    Call Scope1
    R = L2Log
    M36 = R
    Exit Function
EH:
    M36 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M37() As Variant
    On Error GoTo EH
    Dim R As Variant
    L2Log = ""
    Dim o As New LangCls
    R = L2Log
    o.Tag = "a"
    R = R & "|" & L2Log
    M37 = R
    Exit Function
EH:
    M37 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M38() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2) As LangCls
    L2Log = ""
    Set a(1) = New LangCls
    a(1).Tag = "1"
    Erase a
    R = L2Log
    M38 = R
    Exit Function
EH:
    M38 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M39() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As LangCls
    ReDim a(1 To 2)
    L2Log = ""
    Set a(1) = New LangCls
    a(1).Tag = "1"
    Set a(2) = a(1)
    ReDim a(1 To 1)
    R = L2Log
    M39 = R
    Exit Function
EH:
    M39 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M40() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    Dim o As LangCls
    L2Log = ""
    Set o = New LangCls
    o.Tag = "c"
    c.Add o
    Set o = Nothing
    R = L2Log
    c.Remove 1
    R = R & "|" & L2Log
    M40 = R
    Exit Function
EH:
    M40 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M41() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    Dim v As Variant
    v = o.N
    R = TypeName(v)
    v = o.Item(1)
    R = R & TypeName(v)
    v = o.V
    R = R & TypeName(v)
    M41 = R
    Exit Function
EH:
    M41 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M42() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    Dim n As Long
    n = o
    R = n
    M42 = R
    Exit Function
EH:
    M42 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M43() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = o & ""
    M43 = R
    Exit Function
EH:
    M43 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M44() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = CStr(o)
    M44 = R
    Exit Function
EH:
    M44 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M45() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = o = o
    M45 = R
    Exit Function
EH:
    M45 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M46() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As New LangCls
    R = o + 1
    M46 = R
    Exit Function
EH:
    M46 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M47() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = TypeName(New LangCls)
    M47 = R
    Exit Function
EH:
    M47 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M48() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = New LangCls Is Nothing
    M48 = R
    Exit Function
EH:
    M48 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M49() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As LangCls
    Set o = New LangCls
    o.Tag = "abc"
    Dim s As String
    s = o.Tag
    o.Tag = o.Tag & s
    R = o.Tag & Len(o.Tag)
    M49 = R
    Exit Function
EH:
    M49 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M50() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As Object
    Set o = Nothing
    R = TypeName(o) & IsObject(o) & VarType(o)
    M50 = R
    Exit Function
EH:
    M50 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M51() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    Set v = Nothing
    R = TypeName(v) & VarType(v) & IsObject(v) & IsEmpty(v)
    M51 = R
    Exit Function
EH:
    M51 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M52() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As LangCls
    Dim p As LangCls
    R = (o Is p)
    Set o = New LangCls
    R = R & (o Is p)
    M52 = R
    Exit Function
EH:
    M52 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M53() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim o As LangCls
    Set o = New LangCls
    Dim p As LangCls
    Set p = o.Self
    Set o = Nothing
    R = p.N
    M53 = R
    Exit Function
EH:
    M53 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M54() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add "b"
    c.Add 3.5
    R = c.Count & "|" & c(1) & c(2) & c(3) & "|" & TypeName(c(1)) & TypeName(c(3))
    M54 = R
    Exit Function
EH:
    M54 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M55() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add "x", "k1"
    c.Add "y", "K2"
    R = c("k1") & c("K1") & c("k2") & c.Item("K2") & c.Count
    M55 = R
    Exit Function
EH:
    M55 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M56() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add "x", "k"
    c.Add "y", "K"
    M56 = R
    Exit Function
EH:
    M56 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M57() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1, "a"
    c.Add 2, "b"
    c.Add 0, "z", "a"
    c.Add 9, "y", , "b"
    R = c(1) & c(2) & c(3) & c(4)
    M57 = R
    Exit Function
EH:
    M57 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M58() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    c.Add 0, , 1
    c.Add 9, , , 1
    c.Add 5, , 99
    M58 = R
    Exit Function
EH:
    M58 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M59() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1, "a"
    c.Add 2, , 1, 1
    M59 = R
    Exit Function
EH:
    M59 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M60() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1, "a"
    c.Add 2, "b", "zz"
    M60 = R
    Exit Function
EH:
    M60 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M61() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    R = c(2)
    M61 = R
    Exit Function
EH:
    M61 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M62() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    R = c(0)
    M62 = R
    Exit Function
EH:
    M62 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M63() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    R = c("nokey")
    M63 = R
    Exit Function
EH:
    M63 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M64() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    R = c(1.5)
    M64 = R
    Exit Function
EH:
    M64 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M65() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    R = c(1.5)
    M65 = R
    Exit Function
EH:
    M65 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M66() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    R = c(2.5) & c(Empty)
    M66 = R
    Exit Function
EH:
    M66 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M67() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 5, 1
    M67 = R
    Exit Function
EH:
    M67 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M68() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 5, Null
    M68 = R
    Exit Function
EH:
    M68 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M69() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 5, ""
    R = c.Count & c("")
    M69 = R
    Exit Function
EH:
    M69 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M70() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1, "a"
    c.Add 2, "b"
    c.Remove "a"
    R = c.Count & c(1)
    c.Remove 1
    R = R & c.Count
    c.Remove 1
    M70 = R
    Exit Function
EH:
    M70 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M71() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Remove 1
    M71 = R
    Exit Function
EH:
    M71 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M72() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Remove "x"
    M72 = R
    Exit Function
EH:
    M72 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M73() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    c.Add 3
    Dim v As Variant, s As String
    For Each v In c
        s = s & v
    Next v
    R = s
    M73 = R
    Exit Function
EH:
    M73 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M74() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    c.Add 3
    Dim v As Variant, s As String
    For Each v In c
        s = s & v
        If v = 1 Then c.Remove 2
    Next v
    R = s & c.Count
    M74 = R
    Exit Function
EH:
    M74 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M75() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    Dim v As Variant, s As String
    For Each v In c
        s = s & v
        If c.Count < 5 Then c.Add v + 10
    Next v
    R = s & c.Count
    M75 = R
    Exit Function
EH:
    M75 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M76() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    Dim v As Variant, s As String
    For Each v In c
        s = s & "x"
    Next v
    R = "[" & s & "]" & c.Count
    M76 = R
    Exit Function
EH:
    M76 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M77() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add Array(1, 2)
    c.Add Empty
    c.Add Null
    R = TypeName(c(1)) & TypeName(c(2)) & TypeName(c(3)) & c.Count
    M77 = R
    Exit Function
EH:
    M77 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M78() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add Nothing
    M78 = R
    Exit Function
EH:
    M78 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M79() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Collection
    R = c.Count
    M79 = R
    Exit Function
EH:
    M79 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M80() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Collection
    Set c = New Collection
    Dim d As Collection
    Set d = c
    d.Add 1
    R = c.Count & (c Is d)
    M80 = R
    Exit Function
EH:
    M80 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M81() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    Dim d As New Collection
    d.Add c
    R = d(1).Count & d(1)(2) & d(1).Item(1)
    M81 = R
    Exit Function
EH:
    M81 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M82() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    R = TypeName(c) & VarType(c) & (c Is Nothing)
    M82 = R
    Exit Function
EH:
    M82 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M83() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    R = c.Count & c!Nope
    M83 = R
    Exit Function
EH:
    M83 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M84() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1, "k"
    R = c!k
    M84 = R
    Exit Function
EH:
    M84 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M85() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    Dim i As Long
    For i = 1 To 300
        c.Add i, "k" & i
    Next i
    R = c.Count & c("k150") & c(300)
    For i = 1 To 299
        c.Remove 1
    Next i
    R = R & c.Count & c(1)
    M85 = R
    Exit Function
EH:
    M85 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M86() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add "a", "" & ChrW(196) & ""
    R = c("" & ChrW(228) & "") & c.Count
    M86 = R
    Exit Function
EH:
    M86 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M87() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add "a", "ss"
    R = c("SS")
    R = R & c("" & ChrW(223) & "")
    M87 = R
    Exit Function
EH:
    M87 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M88() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add "a", "1"
    R = c("1") & c(1)
    M88 = R
    Exit Function
EH:
    M88 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M89() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add "a", " k"
    R = c(" k")
    R = R & c("k")
    M89 = R
    Exit Function
EH:
    M89 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M90() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    c.Add 3
    c.Remove 2
    c.Add 9, , 2
    R = c(1) & c(2) & c(3)
    M90 = R
    Exit Function
EH:
    M90 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M91() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    c.Add 2
    c.Add 3
    c.Add 9, , 1
    c.Add 8, , , 3
    R = c(1) & c(2) & c(3) & c(4) & c(5)
    M91 = R
    Exit Function
EH:
    M91 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M92() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1
    R = c.Item(Index:=1)
    c.Add Item:=2, Key:="k"
    R = R & c.Item(Index:="k")
    M92 = R
    Exit Function
EH:
    M92 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M93() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    c.Add 1, "a"
    c.Remove Index:="a"
    R = c.Count
    M93 = R
    Exit Function
EH:
    M93 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M94() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    Dim v As Variant
    v = c.Count
    R = TypeName(v) & TypeName(c.Count)
    M94 = R
    Exit Function
EH:
    M94 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M95() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As New Collection
    Dim v As Variant
    c.Add 5
    v = c(1)
    R = TypeName(v) & TypeName(c(1))
    M95 = R
    Exit Function
EH:
    M95 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M96() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim c As Collection
    Set c = New Collection
    c.Add Item:=1, Before:=1
    R = c.Count
    M96 = R
    Exit Function
EH:
    M96 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M97() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    Fill a
    R = Dump(a)
    M97 = R
    Exit Function
EH:
    M97 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M98() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(5) As Long
    R = LBound(a) & UBound(a) & UBound(a, 1)
    M98 = R
    Exit Function
EH:
    M98 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M99() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3, 2 To 4) As Long
    R = LBound(a, 1) & UBound(a, 1) & LBound(a, 2) & UBound(a, 2)
    M99 = R
    Exit Function
EH:
    M99 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M100() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3, 2 To 4) As Long
    R = UBound(a, 3)
    M100 = R
    Exit Function
EH:
    M100 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M101() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    R = UBound(a, 0)
    M101 = R
    Exit Function
EH:
    M101 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M102() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    R = UBound(a)
    M102 = R
    Exit Function
EH:
    M102 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M103() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    R = LBound(a)
    M103 = R
    Exit Function
EH:
    M103 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M104() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(2 To 4)
    R = LBound(a) & UBound(a)
    ReDim a(0)
    R = R & LBound(a) & UBound(a)
    M104 = R
    Exit Function
EH:
    M104 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M105() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(1 To 3)
    a(1) = 1: a(2) = 2: a(3) = 3
    ReDim Preserve a(1 To 5)
    R = Dump(a)
    ReDim Preserve a(1 To 2)
    R = R & Dump(a)
    M105 = R
    Exit Function
EH:
    M105 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M106() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(1 To 2, 1 To 2)
    a(2, 2) = 5
    ReDim Preserve a(1 To 2, 1 To 3)
    R = a(2, 2) & UBound(a, 2)
    ReDim Preserve a(1 To 3, 1 To 3)
    M106 = R
    Exit Function
EH:
    M106 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M107() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(1 To 2)
    a(1) = 1
    ReDim a(1 To 2)
    R = a(1)
    M107 = R
    Exit Function
EH:
    M107 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M108() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As String
    ReDim a(1 To 2)
    R = "[" & a(1) & "]" & TypeName(a(1)) & Len(a(2))
    M108 = R
    Exit Function
EH:
    M108 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M109() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Variant
    ReDim a(1 To 2)
    R = TypeName(a(1)) & TypeName(a)
    M109 = R
    Exit Function
EH:
    M109 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M110() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2) As Variant
    R = TypeName(a) & TypeName(a(1)) & VarType(a) & IsArray(a)
    M110 = R
    Exit Function
EH:
    M110 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M111() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, "b", 3.5)
    R = Dump(a) & LBound(a) & UBound(a)
    M111 = R
    Exit Function
EH:
    M111 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M112() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array()
    R = UBound(a) & LBound(a) & IsArray(a)
    M112 = R
    Exit Function
EH:
    M112 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M113() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(Array(1, 2), Array(3))
    R = a(0)(1) & a(1)(0) & UBound(a(0)) & Dump(a)
    M113 = R
    Exit Function
EH:
    M113 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M114() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2, 3)
    Dim b As Variant
    b = a
    b(0) = 99
    R = a(0) & b(0)
    M114 = R
    Exit Function
EH:
    M114 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M115() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    Dim b() As Long
    a(2) = 7
    b = a
    R = LBound(b) & UBound(b) & b(2)
    M115 = R
    Exit Function
EH:
    M115 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M116() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    a = MkArr()
    R = LBound(a) & UBound(a) & a(3)
    M116 = R
    Exit Function
EH:
    M116 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M117() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = MkArr()
    R = TypeName(v) & LBound(v) & Dump(v)
    M117 = R
    Exit Function
EH:
    M117 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M118() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    a(1) = 5
    Erase a
    R = a(1) & UBound(a)
    M118 = R
    Exit Function
EH:
    M118 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M119() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(1 To 3)
    Erase a
    R = UBound(a)
    M119 = R
    Exit Function
EH:
    M119 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M120() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(1 To 3)
    Erase a
    ReDim a(1 To 2)
    R = UBound(a)
    M120 = R
    Exit Function
EH:
    M120 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M121() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As String
    ReDim a(1 To 2)
    a(1) = "x"
    Erase a
    R = "[" & a(1) & "]"
    M121 = R
    Exit Function
EH:
    M121 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M122() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    R = a(4)
    M122 = R
    Exit Function
EH:
    M122 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M123() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    R = a(0)
    M123 = R
    Exit Function
EH:
    M123 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M124() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    a(4) = 1
    M124 = R
    Exit Function
EH:
    M124 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M125() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    R = a(1.5) & a(2.5)
    M125 = R
    Exit Function
EH:
    M125 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M126() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    R = a("2")
    M126 = R
    Exit Function
EH:
    M126 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M127() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    R = a("x")
    M127 = R
    Exit Function
EH:
    M127 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M128() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    R = a(Null)
    M128 = R
    Exit Function
EH:
    M128 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M129() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim n As Long
    n = -1
    Dim a() As Long
    ReDim a(n)
    M129 = R
    Exit Function
EH:
    M129 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M130() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(0 To -1)
    M130 = R
    Exit Function
EH:
    M130 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M131() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(1 To 2147483647)
    M131 = R
    Exit Function
EH:
    M131 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M132() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(1 To 3) As Long
    R = UBound(a)
    M132 = R
    Exit Function
EH:
    M132 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M133() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    ReDim v(1 To 3)
    v(2) = "x"
    R = Dump(v)
    M133 = R
    Exit Function
EH:
    M133 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M134() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = Array(1, 2)
    Grow v
    R = UBound(v) & Dump(v)
    M134 = R
    Exit Function
EH:
    M134 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M135() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Split("a,b,c", ",")
    R = LBound(a) & UBound(a) & TypeName(a) & Join(a, "-")
    M135 = R
    Exit Function
EH:
    M135 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M136() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Split("", ",")
    R = UBound(a) & "|" & LBound(a)
    M136 = R
    Exit Function
EH:
    M136 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M137() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As String
    a = Split("a b c")
    R = UBound(a) & a(2)
    M137 = R
    Exit Function
EH:
    M137 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M138() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Split("a,b,,c", ",", -1)
    R = UBound(a) & Join(a, "|") & Join(Split("a,b,c", ",", 2), "|")
    M138 = R
    Exit Function
EH:
    M138 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M139() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    R = Join(a, ",")
    M139 = R
    Exit Function
EH:
    M139 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M140() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As String
    a(1) = "p": a(3) = "q"
    R = Join(a, "-")
    M140 = R
    Exit Function
EH:
    M140 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M141() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2, Null)
    R = Join(a, "-")
    M141 = R
    Exit Function
EH:
    M141 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M142() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2, 3)
    R = Join(a)
    M142 = R
    Exit Function
EH:
    M142 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M143() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array("b", "a", "c")
    R = Join(Filter(a, "a"), "|") & Join(Filter(a, "a", False), "|")
    M143 = R
    Exit Function
EH:
    M143 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M144() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array("b", "a", "c")
    R = UBound(Filter(a, "z"))
    M144 = R
    Exit Function
EH:
    M144 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M145() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2, 1 To 2) As Long
    R = IsArray(a) & UBound(a) & TypeName(a)
    M145 = R
    Exit Function
EH:
    M145 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M146() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2, 1 To 2) As Long
    Dim i As Long, j As Long, s As String
    For i = 1 To 2
        For j = 1 To 2
            a(i, j) = i * 10 + j
        Next j
    Next i
    Dim v As Variant
    For Each v In a
        s = s & v & ","
    Next v
    R = s
    M146 = R
    Exit Function
EH:
    M146 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M147() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    a(1) = 1: a(2) = 2: a(3) = 3
    Dim v As Variant, s As String
    For Each v In a
        s = s & v
        a(3) = 9
    Next v
    R = s
    M147 = R
    Exit Function
EH:
    M147 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M148() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    Dim i As Variant
    For Each i In a
        R = R & TypeName(i)
    Next i
    M148 = R
    Exit Function
EH:
    M148 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M149() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    Dim v As Variant
    For Each v In a
        R = "x"
    Next v
    M149 = R
    Exit Function
EH:
    M149 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M150() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(3, 1, 2)
    R = a(UBound(a)) & a(LBound(a)) & a(Z())
    M150 = R
    Exit Function
EH:
    M150 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M151() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, b As Variant
    a = Array(1, 2)
    b = Array(1, 2)
    R = a = b
    M151 = R
    Exit Function
EH:
    M151 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M152() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2)
    R = a + 1
    M152 = R
    Exit Function
EH:
    M152 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M153() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2)
    R = a & "x"
    M153 = R
    Exit Function
EH:
    M153 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M154() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2)
    R = CStr(a)
    M154 = R
    Exit Function
EH:
    M154 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M155() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2)
    R = a
    M155 = R
    Exit Function
EH:
    M155 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M156() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2)
    R = UBound(a) + LBound(a)
    a = 5
    R = R & IsArray(a)
    R = R & UBound(a)
    M156 = R
    Exit Function
EH:
    M156 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M157() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Fa(1, "x", Empty))
    M157 = R
    Exit Function
EH:
    M157 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M158() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = UBound(Fa())
    M158 = R
    Exit Function
EH:
    M158 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M159() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = UBound(Array(1, 2, 3), 1) & LBound(Array(1), 1)
    M159 = R
    Exit Function
EH:
    M159 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M160() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = UBound(Array(1), 2)
    M160 = R
    Exit Function
EH:
    M160 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M161() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long
    Dim b As Variant
    b = a
    b(1) = 5
    R = a(1) & b(1) & LBound(b)
    M161 = R
    Exit Function
EH:
    M161 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M162() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(-2 To 2) As Long
    a(-2) = 1: a(2) = 2
    R = LBound(a) & UBound(a) & a(-2) & a(2)
    M162 = R
    Exit Function
EH:
    M162 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M163() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Byte
    a(1) = 255
    a(1) = a(1) + 1
    M163 = R
    Exit Function
EH:
    M163 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M164() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Byte
    a = "ab"
    R = UBound(a) & a(0) & a(1) & a(2) & a(3)
    M164 = R
    Exit Function
EH:
    M164 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M165() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Byte
    a = "" & ChrW(233) & "" & ChrW(8364) & ""
    R = UBound(a) & "," & a(0) & "," & a(1) & "," & a(2) & "," & a(3)
    M165 = R
    Exit Function
EH:
    M165 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M166() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Byte
    a = StrConv("ab", 128)
    R = UBound(a) & a(0) & a(1)
    M166 = R
    Exit Function
EH:
    M166 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M167() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Byte
    ReDim a(0 To 3)
    a(0) = 97: a(2) = 98
    Dim s As String
    s = a
    R = Len(s) & s
    M167 = R
    Exit Function
EH:
    M167 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M168() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Byte
    ReDim a(0 To 2)
    Dim s As String
    s = a
    R = Len(s) & "|" & LenB(s)
    M168 = R
    Exit Function
EH:
    M168 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M169() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    Dim a() As Byte
    a = s
    ReDim Preserve a(0 To 3)
    s = a
    R = LenB(s) & Len(s)
    M169 = R
    Exit Function
EH:
    M169 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M170() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    ReDim a(1 To 2, 1 To 2)
    a(1, 2) = "x"
    R = Dump(a(1, 2)) & UBound(a, 2)
    M170 = R
    Exit Function
EH:
    M170 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M171() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2) As Long
    Dim p As Variant
    p = a
    R = TypeName(p) & VarType(p)
    M171 = R
    Exit Function
EH:
    M171 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M172() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2) As Long
    R = VarType(a) & TypeName(a)
    M172 = R
    Exit Function
EH:
    M172 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M173() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    R = VarType(a) & TypeName(a) & IsArray(a)
    M173 = R
    Exit Function
EH:
    M173 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M174() As Variant
    On Error GoTo EH
    Dim R As Variant
    Static a() As Long
    ReDim a(1 To 2)
    R = UBound(a)
    M174 = R
    Exit Function
EH:
    M174 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M175() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long, b() As Long
    ReDim a(1 To 2)
    b = a
    ReDim a(1 To 5)
    R = UBound(b) & UBound(a)
    M175 = R
    Exit Function
EH:
    M175 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M176() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a() As Long
    ReDim a(1 To 2)
    a(1) = 11
    Dim b As Variant
    b = a
    ReDim a(1 To 2)
    R = b(1) & a(1)
    M176 = R
    Exit Function
EH:
    M176 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M177() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = Array(1, 2, 3)
    Dim i As Long, s As String
    For i = UBound(a) To LBound(a) Step -1
        s = s & a(i)
    Next i
    R = s
    M177 = R
    Exit Function
EH:
    M177 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M178() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2) As Long
    Dim i As Long
    On Error Resume Next
    For i = 0 To 3
        a(i) = i
        R = R & Err.Number & ","
        Err.Clear
    Next i
    M178 = R
    Exit Function
EH:
    M178 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M179() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M179 = R
    Exit Function
EH:
    M179 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M180() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M180 = R
    Exit Function
EH:
    M180 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M181() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(-1)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M181 = R
    Exit Function
EH:
    M181 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M182() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.1)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M182 = R
    Exit Function
EH:
    M182 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M183() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.5)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M183 = R
    Exit Function
EH:
    M183 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M184() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1.5)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M184 = R
    Exit Function
EH:
    M184 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M185() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(100)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M185 = R
    Exit Function
EH:
    M185 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M186() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E5)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M186 = R
    Exit Function
EH:
    M186 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M187() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E14)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M187 = R
    Exit Function
EH:
    M187 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M188() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E15)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M188 = R
    Exit Function
EH:
    M188 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M189() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E16)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M189 = R
    Exit Function
EH:
    M189 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M190() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(123456789012345)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M190 = R
    Exit Function
EH:
    M190 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M191() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1234567890123456)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M191 = R
    Exit Function
EH:
    M191 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M192() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(12345678901234567)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M192 = R
    Exit Function
EH:
    M192 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M193() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.1 + 0.2)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M193 = R
    Exit Function
EH:
    M193 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M194() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1 / 3)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M194 = R
    Exit Function
EH:
    M194 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M195() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(2 / 3)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M195 = R
    Exit Function
EH:
    M195 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M196() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(10 / 3)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M196 = R
    Exit Function
EH:
    M196 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M197() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(100 / 3)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M197 = R
    Exit Function
EH:
    M197 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M198() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E-4)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M198 = R
    Exit Function
EH:
    M198 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M199() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E-5)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M199 = R
    Exit Function
EH:
    M199 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M200() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.00001234)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M200 = R
    Exit Function
EH:
    M200 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M201() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1.5E-10)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M201 = R
    Exit Function
EH:
    M201 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M202() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(123.456E-7)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M202 = R
    Exit Function
EH:
    M202 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M203() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E100)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M203 = R
    Exit Function
EH:
    M203 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M204() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1.7976931348623157E+308)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M204 = R
    Exit Function
EH:
    M204 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M205() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(4.94065645841247E-324)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M205 = R
    Exit Function
EH:
    M205 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M206() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(2.2250738585072014E-308)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M206 = R
    Exit Function
EH:
    M206 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M207() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(9007199254740993)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M207 = R
    Exit Function
EH:
    M207 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M208() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.3)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M208 = R
    Exit Function
EH:
    M208 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M209() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.7)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M209 = R
    Exit Function
EH:
    M209 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M210() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1.1)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M210 = R
    Exit Function
EH:
    M210 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M211() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(2.675)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M211 = R
    Exit Function
EH:
    M211 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M212() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1.005)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M212 = R
    Exit Function
EH:
    M212 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M213() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(99999999999999.9)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M213 = R
    Exit Function
EH:
    M213 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M214() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(999999999999999.9)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M214 = R
    Exit Function
EH:
    M214 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M215() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(9.99999999999999E+14)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M215 = R
    Exit Function
EH:
    M215 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M216() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1234.5678)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M216 = R
    Exit Function
EH:
    M216 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M217() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(-0.000123)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M217 = R
    Exit Function
EH:
    M217 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M218() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(3.14159265358979)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M218 = R
    Exit Function
EH:
    M218 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M219() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(2.718281828459045)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M219 = R
    Exit Function
EH:
    M219 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M220() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E21)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M220 = R
    Exit Function
EH:
    M220 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M221() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E22)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M221 = R
    Exit Function
EH:
    M221 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M222() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(123456789.123456789)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M222 = R
    Exit Function
EH:
    M222 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M223() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.000001)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M223 = R
    Exit Function
EH:
    M223 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M224() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.0000001)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M224 = R
    Exit Function
EH:
    M224 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M225() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(5E-324)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M225 = R
    Exit Function
EH:
    M225 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M226() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(8.5)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M226 = R
    Exit Function
EH:
    M226 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M227() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(9.5)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M227 = R
    Exit Function
EH:
    M227 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M228() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(4.35)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M228 = R
    Exit Function
EH:
    M228 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M229() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.045)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M229 = R
    Exit Function
EH:
    M229 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M230() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1/7)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M230 = R
    Exit Function
EH:
    M230 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M231() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(22/7)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M231 = R
    Exit Function
EH:
    M231 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M232() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E-15)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M232 = R
    Exit Function
EH:
    M232 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M233() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(3E-7)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M233 = R
    Exit Function
EH:
    M233 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M234() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(65536 * 65536)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M234 = R
    Exit Function
EH:
    M234 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M235() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(2 ^ 53)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M235 = R
    Exit Function
EH:
    M235 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M236() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(2 ^ 63)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M236 = R
    Exit Function
EH:
    M236 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M237() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(2 ^ 64)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M237 = R
    Exit Function
EH:
    M237 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M238() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(-2 ^ 31)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M238 = R
    Exit Function
EH:
    M238 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M239() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E15 + 0.5)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M239 = R
    Exit Function
EH:
    M239 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M240() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1E15 + 0.3)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M240 = R
    Exit Function
EH:
    M240 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M241() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(123456.7)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M241 = R
    Exit Function
EH:
    M241 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M242() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1234567.8)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M242 = R
    Exit Function
EH:
    M242 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M243() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(0.1 * 3)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M243 = R
    Exit Function
EH:
    M243 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M244() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = fD(1 - 0.9)
    R = CStr(d) & "|" & Str(d) & "|" & d & "|" & Format(d) & "|" & Val(CStr(d)) - d
    M244 = R
    Exit Function
EH:
    M244 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M245() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(0)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M245 = R
    Exit Function
EH:
    M245 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M246() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M246 = R
    Exit Function
EH:
    M246 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M247() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(0.1)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M247 = R
    Exit Function
EH:
    M247 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M248() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(0.5)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M248 = R
    Exit Function
EH:
    M248 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M249() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1.5)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M249 = R
    Exit Function
EH:
    M249 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M250() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1E7)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M250 = R
    Exit Function
EH:
    M250 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M251() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1E8)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M251 = R
    Exit Function
EH:
    M251 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M252() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(16777216)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M252 = R
    Exit Function
EH:
    M252 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M253() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(16777217)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M253 = R
    Exit Function
EH:
    M253 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M254() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(123456789)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M254 = R
    Exit Function
EH:
    M254 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M255() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(0.3)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M255 = R
    Exit Function
EH:
    M255 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M256() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(3.14159265)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M256 = R
    Exit Function
EH:
    M256 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M257() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1/3)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M257 = R
    Exit Function
EH:
    M257 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M258() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(2/3)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M258 = R
    Exit Function
EH:
    M258 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M259() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1E-4)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M259 = R
    Exit Function
EH:
    M259 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M260() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1E-5)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M260 = R
    Exit Function
EH:
    M260 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M261() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1.5E-10)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M261 = R
    Exit Function
EH:
    M261 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M262() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(3.4028235E+38)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M262 = R
    Exit Function
EH:
    M262 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M263() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1.401298E-45)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M263 = R
    Exit Function
EH:
    M263 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M264() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1.17549435E-38)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M264 = R
    Exit Function
EH:
    M264 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M265() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(0.1 + 0.2)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M265 = R
    Exit Function
EH:
    M265 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M266() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(100 / 3)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M266 = R
    Exit Function
EH:
    M266 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M267() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(99999.99)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M267 = R
    Exit Function
EH:
    M267 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M268() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(9999999)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M268 = R
    Exit Function
EH:
    M268 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M269() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(12345.678)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M269 = R
    Exit Function
EH:
    M269 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M270() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1234567.8)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M270 = R
    Exit Function
EH:
    M270 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M271() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(-0.00123)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M271 = R
    Exit Function
EH:
    M271 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M272() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(7777777)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M272 = R
    Exit Function
EH:
    M272 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M273() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(8388608.5)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M273 = R
    Exit Function
EH:
    M273 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M274() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(0.7)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M274 = R
    Exit Function
EH:
    M274 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M275() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1.1)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M275 = R
    Exit Function
EH:
    M275 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M276() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(2.675)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M276 = R
    Exit Function
EH:
    M276 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M277() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(4.35)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M277 = R
    Exit Function
EH:
    M277 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M278() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1E10)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M278 = R
    Exit Function
EH:
    M278 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M279() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(1.23456789E-5)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M279 = R
    Exit Function
EH:
    M279 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M280() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(33554432)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M280 = R
    Exit Function
EH:
    M280 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M281() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Single
    s = fS(0.000123456)
    R = CStr(s) & "|" & Str(s) & "|" & s & "|" & CDbl(s) & "|" & Val(CStr(s)) - s
    M281 = R
    Exit Function
EH:
    M281 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M282() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CCur(1.5)) & "|" & CCur(1.5) & "|" & Str(CCur(1.5))
    M282 = R
    Exit Function
EH:
    M282 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M283() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CCur(123456789012.3456)) & "|" & CCur(123456789012.3456) & "|" & Str(CCur(123456789012.3456))
    M283 = R
    Exit Function
EH:
    M283 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M284() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CCur(-0.0001)) & "|" & CCur(-0.0001) & "|" & Str(CCur(-0.0001))
    M284 = R
    Exit Function
EH:
    M284 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M285() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CDec(1) / 3) & "|" & CDec(1) / 3 & "|" & Str(CDec(1) / 3)
    M285 = R
    Exit Function
EH:
    M285 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M286() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CDec("1.10")) & "|" & CDec("1.10") & "|" & Str(CDec("1.10"))
    M286 = R
    Exit Function
EH:
    M286 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M287() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CDec(1E15)) & "|" & CDec(1E15) & "|" & Str(CDec(1E15))
    M287 = R
    Exit Function
EH:
    M287 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M288() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CDec(0.1) * 3) & "|" & CDec(0.1) * 3 & "|" & Str(CDec(0.1) * 3)
    M288 = R
    Exit Function
EH:
    M288 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M289() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CDec("79228162514264337593543950335")) & "|" & CDec("79228162514264337593543950335") & "|" & Str(CDec("79228162514264337593543950335"))
    M289 = R
    Exit Function
EH:
    M289 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M290() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CByte(200)) & "|" & CByte(200) & "|" & Str(CByte(200))
    M290 = R
    Exit Function
EH:
    M290 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M291() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CInt(-5)) & "|" & CInt(-5) & "|" & Str(CInt(-5))
    M291 = R
    Exit Function
EH:
    M291 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M292() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CLng(2147483647)) & "|" & CLng(2147483647) & "|" & Str(CLng(2147483647))
    M292 = R
    Exit Function
EH:
    M292 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M293() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(True) & "|" & True & "|" & Str(True)
    M293 = R
    Exit Function
EH:
    M293 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M294() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CDate(0.5)) & "|" & CDate(0.5) & "|" & Str(CDate(0.5))
    M294 = R
    Exit Function
EH:
    M294 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M295() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CDate(36526.25)) & "|" & CDate(36526.25) & "|" & Str(CDate(36526.25))
    M295 = R
    Exit Function
EH:
    M295 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M296() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(#12/31/1999 11:59:59 PM#) & "|" & #12/31/1999 11:59:59 PM# & "|" & Str(#12/31/1999 11:59:59 PM#)
    M296 = R
    Exit Function
EH:
    M296 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M297() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CDate(-0.5)) & "|" & CDate(-0.5) & "|" & Str(CDate(-0.5))
    M297 = R
    Exit Function
EH:
    M297 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M298() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(Empty) & "|" & Empty & "|" & Str(Empty)
    M298 = R
    Exit Function
EH:
    M298 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M299() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CVErr(7)) & "|" & CVErr(7) & "|" & Str(CVErr(7))
    M299 = R
    Exit Function
EH:
    M299 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M300() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Str(5) & "|" & Str(-5) & "|" & Str(0) & "|" & Str(1.5) & "|" & Str(True) & "|" & Str("5")
    M300 = R
    Exit Function
EH:
    M300 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M301() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Str(Null)
    M301 = R
    Exit Function
EH:
    M301 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M302() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Str(Empty) & "|" & Str(#1/1/2000#)
    M302 = R
    Exit Function
EH:
    M302 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M303() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Str("abc")
    M303 = R
    Exit Function
EH:
    M303 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M304() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Str(fS(0.1)) & Str(fD(0.1)) & Str(CCur(1.5)) & Str(CDec(1.5))
    M304 = R
    Exit Function
EH:
    M304 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M305() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Str(1E15) & "|" & Str(1E16) & "|" & Str(-1E-5)
    M305 = R
    Exit Function
EH:
    M305 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M306() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Str(1000000) & "|" & Str(123456789012#)
    M306 = R
    Exit Function
EH:
    M306 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M307() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(1000000) & "|" & CStr(1E6) & "|" & CStr(1000000#) & "|" & CStr(1000000!)
    M307 = R
    Exit Function
EH:
    M307 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M308() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(0.1!) & "|" & CStr(0.1) & "|" & CStr(0.1@) & "|" & CStr(0.1 + 0.2!)
    M308 = R
    Exit Function
EH:
    M308 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M309() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CSng(0.1) & "|" & CDbl(CSng(0.1)) & "|" & CSng(1E10) & "|" & CSng(123456789)
    M309 = R
    Exit Function
EH:
    M309 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M310() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(CSng(16777217)) & "|" & CStr(CSng(0.1) * 3) & "|" & CStr(CSng(1) / 3)
    M310 = R
    Exit Function
EH:
    M310 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M311() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Hex(255) & Oct(8) & Hex(-1) & Hex(CDbl(255.5)) & Hex(65536)
    M311 = R
    Exit Function
EH:
    M311 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M312() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Val("1.5E3") & "|" & Val("1E-3") & "|" & Val(".5") & "|" & Val("5.") & "|" & Val("1e400")
    M312 = R
    Exit Function
EH:
    M312 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M313() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CDbl("1.7976931348623157E308") & "|" & CDbl("4.94065645841247E-324")
    M313 = R
    Exit Function
EH:
    M313 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M314() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = CStr(1 / 3) & "|" & (1 / 3) & "|" & Str(1 / 3) & "|" & Format(1 / 3, "0.000000000000000000")
    M314 = R
    Exit Function
EH:
    M314 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M315() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Fix(1E15 + 0.5) & "|" & Int(2 ^ 52 + 0.5) & "|" & CStr(2 ^ 52 + 0.5)
    M315 = R
    Exit Function
EH:
    M315 = "#ERR " & Err.Number & " " & Err.Description
End Function
