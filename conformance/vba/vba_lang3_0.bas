Attribute VB_Name = "VbaLang3_0"
Option Explicit

Public L3Log As String
Public L2Log As String
Public Enum Color3
    Red3 = 1
    Green3 = 5
    Blue3
    Neg3 = -2
End Enum
Public Type Pt3
    X As Long
    Y As Double
    S As String
    A(1 To 3) As Integer
End Type
Public Type Box3
    P As Pt3
    Tag As String
End Type
Private Function fV(x As Variant) As Variant
    fV = x
End Function
Private Function Z() As Long
    Z = 0
End Function
Private Sub ByRefL(ByRef n As Long)
    n = n + 1
End Sub
Private Sub ByValL(ByVal n As Long)
    n = n + 1
End Sub
Private Sub ByRefV(ByRef v As Variant)
    v = v & "x"
End Sub
Private Sub ByRefI(ByRef n As Integer)
    n = n + 1
End Sub
Private Function Opt1(Optional a As Variant, Optional b As Long = 7, Optional c As String = "d") As String
    Opt1 = IIf(IsMissing(a), "M", "a=" & a) & "|" & b & "|" & c
End Function
Private Function Opt2(Optional a As Long, Optional b As Variant = Empty) As String
    Opt2 = a & "|" & TypeName(b)
End Function
Private Function PA(ParamArray p() As Variant) As String
    Dim i As Long, s As String
    s = LBound(p) & ":" & UBound(p) & ":"
    For i = LBound(p) To UBound(p)
        s = s & TypeName(p(i)) & ","
    Next i
    PA = s
End Function
Private Function PA2(ByVal a As Long, ParamArray p() As Variant) As String
    PA2 = a & ":" & (UBound(p) - LBound(p) + 1)
End Function
Private Function StatCnt() As Long
    Static n As Long
    n = n + 1
    StatCnt = n
End Function
Private Static Function StatAll() As String
    Dim q As Long
    q = q + 1
    StatAll = CStr(q)
End Function
Private Property Get PG() As Long
    PG = 42
End Property
Private Function Gs(ByVal n As Long) As String
    Dim s As String
    s = "a"
    GoSub Sub1
    s = s & "b"
    If n = 1 Then GoSub Sub2
    Gs = s
    Exit Function
Sub1:
    s = s & "1"
    Return
Sub2:
    s = s & "2"
    Return
End Function
Public Function PubF(ByVal a As Long, Optional ByVal b As Long = 3) As Long
    PubF = a * 10 + b
End Function
Public Sub PubS(ByRef a As Long)
    a = a + 100
End Sub
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

Public Function LangThree0(ByVal k As Long) As Variant
    Select Case (k - 0) \ 100
    Case 0: LangThree0 = MC0(k)
    Case 1: LangThree0 = MC1(k)
    Case 2: LangThree0 = MC2(k)
    Case 3: LangThree0 = MC3(k)
    Case 4: LangThree0 = MC4(k)
    Case 5: LangThree0 = MC5(k)
    Case 6: LangThree0 = MC6(k)
    Case 7: LangThree0 = MC7(k)
    Case 8: LangThree0 = MC8(k)
    Case 9: LangThree0 = MC9(k)
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
    Case 317: MC3 = M317()
    Case 318: MC3 = M318()
    Case 319: MC3 = M319()
    Case 320: MC3 = M320()
    Case 321: MC3 = M321()
    Case 322: MC3 = M322()
    Case 323: MC3 = M323()
    Case 324: MC3 = M324()
    Case 325: MC3 = M325()
    Case 326: MC3 = M326()
    Case 327: MC3 = M327()
    Case 328: MC3 = M328()
    Case 329: MC3 = M329()
    Case 330: MC3 = M330()
    Case 331: MC3 = M331()
    Case 332: MC3 = M332()
    Case 333: MC3 = M333()
    Case 334: MC3 = M334()
    Case 335: MC3 = M335()
    Case 336: MC3 = M336()
    Case 337: MC3 = M337()
    Case 338: MC3 = M338()
    Case 339: MC3 = M339()
    Case 340: MC3 = M340()
    Case 341: MC3 = M341()
    Case 342: MC3 = M342()
    Case 343: MC3 = M343()
    Case 344: MC3 = M344()
    Case 345: MC3 = M345()
    Case 346: MC3 = M346()
    Case 347: MC3 = M347()
    Case 348: MC3 = M348()
    Case 349: MC3 = M349()
    Case 350: MC3 = M350()
    Case 351: MC3 = M351()
    Case 352: MC3 = M352()
    Case 353: MC3 = M353()
    Case 354: MC3 = M354()
    Case 355: MC3 = M355()
    Case 356: MC3 = M356()
    Case 357: MC3 = M357()
    Case 358: MC3 = M358()
    Case 359: MC3 = M359()
    Case 360: MC3 = M360()
    Case 361: MC3 = M361()
    Case 362: MC3 = M362()
    Case 363: MC3 = M363()
    Case 364: MC3 = M364()
    Case 365: MC3 = M365()
    Case 366: MC3 = M366()
    Case 367: MC3 = M367()
    Case 368: MC3 = M368()
    Case 369: MC3 = M369()
    Case 370: MC3 = M370()
    Case 371: MC3 = M371()
    Case 372: MC3 = M372()
    Case 373: MC3 = M373()
    Case 374: MC3 = M374()
    Case 375: MC3 = M375()
    Case 376: MC3 = M376()
    Case 377: MC3 = M377()
    Case 378: MC3 = M378()
    Case 379: MC3 = M379()
    Case 380: MC3 = M380()
    Case 381: MC3 = M381()
    Case 382: MC3 = M382()
    Case 383: MC3 = M383()
    Case 384: MC3 = M384()
    Case 385: MC3 = M385()
    Case 386: MC3 = M386()
    Case 387: MC3 = M387()
    Case 388: MC3 = M388()
    Case 389: MC3 = M389()
    Case 390: MC3 = M390()
    Case 391: MC3 = M391()
    Case 392: MC3 = M392()
    Case 393: MC3 = M393()
    Case 394: MC3 = M394()
    Case 395: MC3 = M395()
    Case 396: MC3 = M396()
    Case 397: MC3 = M397()
    Case 398: MC3 = M398()
    Case 399: MC3 = M399()
    End Select
    Exit Function
EH:
    MC3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC4(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 400: MC4 = M400()
    Case 401: MC4 = M401()
    Case 402: MC4 = M402()
    Case 403: MC4 = M403()
    Case 404: MC4 = M404()
    Case 405: MC4 = M405()
    Case 406: MC4 = M406()
    Case 407: MC4 = M407()
    Case 408: MC4 = M408()
    Case 409: MC4 = M409()
    Case 410: MC4 = M410()
    Case 411: MC4 = M411()
    Case 412: MC4 = M412()
    Case 413: MC4 = M413()
    Case 414: MC4 = M414()
    Case 415: MC4 = M415()
    Case 416: MC4 = M416()
    Case 417: MC4 = M417()
    Case 418: MC4 = M418()
    Case 419: MC4 = M419()
    Case 420: MC4 = M420()
    Case 421: MC4 = M421()
    Case 422: MC4 = M422()
    Case 423: MC4 = M423()
    Case 424: MC4 = M424()
    Case 425: MC4 = M425()
    Case 426: MC4 = M426()
    Case 427: MC4 = M427()
    Case 428: MC4 = M428()
    Case 429: MC4 = M429()
    Case 430: MC4 = M430()
    Case 431: MC4 = M431()
    Case 432: MC4 = M432()
    Case 433: MC4 = M433()
    Case 434: MC4 = M434()
    Case 435: MC4 = M435()
    Case 436: MC4 = M436()
    Case 437: MC4 = M437()
    Case 438: MC4 = M438()
    Case 439: MC4 = M439()
    Case 440: MC4 = M440()
    Case 441: MC4 = M441()
    Case 442: MC4 = M442()
    Case 443: MC4 = M443()
    Case 444: MC4 = M444()
    Case 445: MC4 = M445()
    Case 446: MC4 = M446()
    Case 447: MC4 = M447()
    Case 448: MC4 = M448()
    Case 449: MC4 = M449()
    Case 450: MC4 = M450()
    Case 451: MC4 = M451()
    Case 452: MC4 = M452()
    Case 453: MC4 = M453()
    Case 454: MC4 = M454()
    Case 455: MC4 = M455()
    Case 456: MC4 = M456()
    Case 457: MC4 = M457()
    Case 458: MC4 = M458()
    Case 459: MC4 = M459()
    Case 460: MC4 = M460()
    Case 461: MC4 = M461()
    Case 462: MC4 = M462()
    Case 463: MC4 = M463()
    Case 464: MC4 = M464()
    Case 465: MC4 = M465()
    Case 466: MC4 = M466()
    Case 467: MC4 = M467()
    Case 468: MC4 = M468()
    Case 469: MC4 = M469()
    Case 470: MC4 = M470()
    Case 471: MC4 = M471()
    Case 472: MC4 = M472()
    Case 473: MC4 = M473()
    Case 474: MC4 = M474()
    Case 475: MC4 = M475()
    Case 476: MC4 = M476()
    Case 477: MC4 = M477()
    Case 478: MC4 = M478()
    Case 479: MC4 = M479()
    Case 480: MC4 = M480()
    Case 481: MC4 = M481()
    Case 482: MC4 = M482()
    Case 483: MC4 = M483()
    Case 484: MC4 = M484()
    Case 485: MC4 = M485()
    Case 486: MC4 = M486()
    Case 487: MC4 = M487()
    Case 488: MC4 = M488()
    Case 489: MC4 = M489()
    Case 490: MC4 = M490()
    Case 491: MC4 = M491()
    Case 492: MC4 = M492()
    Case 493: MC4 = M493()
    Case 494: MC4 = M494()
    Case 495: MC4 = M495()
    Case 496: MC4 = M496()
    Case 497: MC4 = M497()
    Case 498: MC4 = M498()
    Case 499: MC4 = M499()
    End Select
    Exit Function
EH:
    MC4 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC5(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 500: MC5 = M500()
    Case 501: MC5 = M501()
    Case 502: MC5 = M502()
    Case 503: MC5 = M503()
    Case 504: MC5 = M504()
    Case 505: MC5 = M505()
    Case 506: MC5 = M506()
    Case 507: MC5 = M507()
    Case 508: MC5 = M508()
    Case 509: MC5 = M509()
    Case 510: MC5 = M510()
    Case 511: MC5 = M511()
    Case 512: MC5 = M512()
    Case 513: MC5 = M513()
    Case 514: MC5 = M514()
    Case 515: MC5 = M515()
    Case 516: MC5 = M516()
    Case 517: MC5 = M517()
    Case 518: MC5 = M518()
    Case 519: MC5 = M519()
    Case 520: MC5 = M520()
    Case 521: MC5 = M521()
    Case 522: MC5 = M522()
    Case 523: MC5 = M523()
    Case 524: MC5 = M524()
    Case 525: MC5 = M525()
    Case 526: MC5 = M526()
    Case 527: MC5 = M527()
    Case 528: MC5 = M528()
    Case 529: MC5 = M529()
    Case 530: MC5 = M530()
    Case 531: MC5 = M531()
    Case 532: MC5 = M532()
    Case 533: MC5 = M533()
    Case 534: MC5 = M534()
    Case 535: MC5 = M535()
    Case 536: MC5 = M536()
    Case 537: MC5 = M537()
    Case 538: MC5 = M538()
    Case 539: MC5 = M539()
    Case 540: MC5 = M540()
    Case 541: MC5 = M541()
    Case 542: MC5 = M542()
    Case 543: MC5 = M543()
    Case 544: MC5 = M544()
    Case 545: MC5 = M545()
    Case 546: MC5 = M546()
    Case 547: MC5 = M547()
    Case 548: MC5 = M548()
    Case 549: MC5 = M549()
    Case 550: MC5 = M550()
    Case 551: MC5 = M551()
    Case 552: MC5 = M552()
    Case 553: MC5 = M553()
    Case 554: MC5 = M554()
    Case 555: MC5 = M555()
    Case 556: MC5 = M556()
    Case 557: MC5 = M557()
    Case 558: MC5 = M558()
    Case 559: MC5 = M559()
    Case 560: MC5 = M560()
    Case 561: MC5 = M561()
    Case 562: MC5 = M562()
    Case 563: MC5 = M563()
    Case 564: MC5 = M564()
    Case 565: MC5 = M565()
    Case 566: MC5 = M566()
    Case 567: MC5 = M567()
    Case 568: MC5 = M568()
    Case 569: MC5 = M569()
    Case 570: MC5 = M570()
    Case 571: MC5 = M571()
    Case 572: MC5 = M572()
    Case 573: MC5 = M573()
    Case 574: MC5 = M574()
    Case 575: MC5 = M575()
    Case 576: MC5 = M576()
    Case 577: MC5 = M577()
    Case 578: MC5 = M578()
    Case 579: MC5 = M579()
    Case 580: MC5 = M580()
    Case 581: MC5 = M581()
    Case 582: MC5 = M582()
    Case 583: MC5 = M583()
    Case 584: MC5 = M584()
    Case 585: MC5 = M585()
    Case 586: MC5 = M586()
    Case 587: MC5 = M587()
    Case 588: MC5 = M588()
    Case 589: MC5 = M589()
    Case 590: MC5 = M590()
    Case 591: MC5 = M591()
    Case 592: MC5 = M592()
    Case 593: MC5 = M593()
    Case 594: MC5 = M594()
    Case 595: MC5 = M595()
    Case 596: MC5 = M596()
    Case 597: MC5 = M597()
    Case 598: MC5 = M598()
    Case 599: MC5 = M599()
    End Select
    Exit Function
EH:
    MC5 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC6(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 600: MC6 = M600()
    Case 601: MC6 = M601()
    Case 602: MC6 = M602()
    Case 603: MC6 = M603()
    Case 604: MC6 = M604()
    Case 605: MC6 = M605()
    Case 606: MC6 = M606()
    Case 607: MC6 = M607()
    Case 608: MC6 = M608()
    Case 609: MC6 = M609()
    Case 610: MC6 = M610()
    Case 611: MC6 = M611()
    Case 612: MC6 = M612()
    Case 613: MC6 = M613()
    Case 614: MC6 = M614()
    Case 615: MC6 = M615()
    Case 616: MC6 = M616()
    Case 617: MC6 = M617()
    Case 618: MC6 = M618()
    Case 619: MC6 = M619()
    Case 620: MC6 = M620()
    Case 621: MC6 = M621()
    Case 622: MC6 = M622()
    Case 623: MC6 = M623()
    Case 624: MC6 = M624()
    Case 625: MC6 = M625()
    Case 626: MC6 = M626()
    Case 627: MC6 = M627()
    Case 628: MC6 = M628()
    Case 629: MC6 = M629()
    Case 630: MC6 = M630()
    Case 631: MC6 = M631()
    Case 632: MC6 = M632()
    Case 633: MC6 = M633()
    Case 634: MC6 = M634()
    Case 635: MC6 = M635()
    Case 636: MC6 = M636()
    Case 637: MC6 = M637()
    Case 638: MC6 = M638()
    Case 639: MC6 = M639()
    Case 640: MC6 = M640()
    Case 641: MC6 = M641()
    Case 642: MC6 = M642()
    Case 643: MC6 = M643()
    Case 644: MC6 = M644()
    Case 645: MC6 = M645()
    Case 646: MC6 = M646()
    Case 647: MC6 = M647()
    Case 648: MC6 = M648()
    Case 649: MC6 = M649()
    Case 650: MC6 = M650()
    Case 651: MC6 = M651()
    Case 652: MC6 = M652()
    Case 653: MC6 = M653()
    Case 654: MC6 = M654()
    Case 655: MC6 = M655()
    Case 656: MC6 = M656()
    Case 657: MC6 = M657()
    Case 658: MC6 = M658()
    Case 659: MC6 = M659()
    Case 660: MC6 = M660()
    Case 661: MC6 = M661()
    Case 662: MC6 = M662()
    Case 663: MC6 = M663()
    Case 664: MC6 = M664()
    Case 665: MC6 = M665()
    Case 666: MC6 = M666()
    Case 667: MC6 = M667()
    Case 668: MC6 = M668()
    Case 669: MC6 = M669()
    Case 670: MC6 = M670()
    Case 671: MC6 = M671()
    Case 672: MC6 = M672()
    Case 673: MC6 = M673()
    Case 674: MC6 = M674()
    Case 675: MC6 = M675()
    Case 676: MC6 = M676()
    Case 677: MC6 = M677()
    Case 678: MC6 = M678()
    Case 679: MC6 = M679()
    Case 680: MC6 = M680()
    Case 681: MC6 = M681()
    Case 682: MC6 = M682()
    Case 683: MC6 = M683()
    Case 684: MC6 = M684()
    Case 685: MC6 = M685()
    Case 686: MC6 = M686()
    Case 687: MC6 = M687()
    Case 688: MC6 = M688()
    Case 689: MC6 = M689()
    Case 690: MC6 = M690()
    Case 691: MC6 = M691()
    Case 692: MC6 = M692()
    Case 693: MC6 = M693()
    Case 694: MC6 = M694()
    Case 695: MC6 = M695()
    Case 696: MC6 = M696()
    Case 697: MC6 = M697()
    Case 698: MC6 = M698()
    Case 699: MC6 = M699()
    End Select
    Exit Function
EH:
    MC6 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC7(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 700: MC7 = M700()
    Case 701: MC7 = M701()
    Case 702: MC7 = M702()
    Case 703: MC7 = M703()
    Case 704: MC7 = M704()
    Case 705: MC7 = M705()
    Case 706: MC7 = M706()
    Case 707: MC7 = M707()
    Case 708: MC7 = M708()
    Case 709: MC7 = M709()
    Case 710: MC7 = M710()
    Case 711: MC7 = M711()
    Case 712: MC7 = M712()
    Case 713: MC7 = M713()
    Case 714: MC7 = M714()
    Case 715: MC7 = M715()
    Case 716: MC7 = M716()
    Case 717: MC7 = M717()
    Case 718: MC7 = M718()
    Case 719: MC7 = M719()
    Case 720: MC7 = M720()
    Case 721: MC7 = M721()
    Case 722: MC7 = M722()
    Case 723: MC7 = M723()
    Case 724: MC7 = M724()
    Case 725: MC7 = M725()
    Case 726: MC7 = M726()
    Case 727: MC7 = M727()
    Case 728: MC7 = M728()
    Case 729: MC7 = M729()
    Case 730: MC7 = M730()
    Case 731: MC7 = M731()
    Case 732: MC7 = M732()
    Case 733: MC7 = M733()
    Case 734: MC7 = M734()
    Case 735: MC7 = M735()
    Case 736: MC7 = M736()
    Case 737: MC7 = M737()
    Case 738: MC7 = M738()
    Case 739: MC7 = M739()
    Case 740: MC7 = M740()
    Case 741: MC7 = M741()
    Case 742: MC7 = M742()
    Case 743: MC7 = M743()
    Case 744: MC7 = M744()
    Case 745: MC7 = M745()
    Case 746: MC7 = M746()
    Case 747: MC7 = M747()
    Case 748: MC7 = M748()
    Case 749: MC7 = M749()
    Case 750: MC7 = M750()
    Case 751: MC7 = M751()
    Case 752: MC7 = M752()
    Case 753: MC7 = M753()
    Case 754: MC7 = M754()
    Case 755: MC7 = M755()
    Case 756: MC7 = M756()
    Case 757: MC7 = M757()
    Case 758: MC7 = M758()
    Case 759: MC7 = M759()
    Case 760: MC7 = M760()
    Case 761: MC7 = M761()
    Case 762: MC7 = M762()
    Case 763: MC7 = M763()
    Case 764: MC7 = M764()
    Case 765: MC7 = M765()
    Case 766: MC7 = M766()
    Case 767: MC7 = M767()
    Case 768: MC7 = M768()
    Case 769: MC7 = M769()
    Case 770: MC7 = M770()
    Case 771: MC7 = M771()
    Case 772: MC7 = M772()
    Case 773: MC7 = M773()
    Case 774: MC7 = M774()
    Case 775: MC7 = M775()
    Case 776: MC7 = M776()
    Case 777: MC7 = M777()
    Case 778: MC7 = M778()
    Case 779: MC7 = M779()
    Case 780: MC7 = M780()
    Case 781: MC7 = M781()
    Case 782: MC7 = M782()
    Case 783: MC7 = M783()
    Case 784: MC7 = M784()
    Case 785: MC7 = M785()
    Case 786: MC7 = M786()
    Case 787: MC7 = M787()
    Case 788: MC7 = M788()
    Case 789: MC7 = M789()
    Case 790: MC7 = M790()
    Case 791: MC7 = M791()
    Case 792: MC7 = M792()
    Case 793: MC7 = M793()
    Case 794: MC7 = M794()
    Case 795: MC7 = M795()
    Case 796: MC7 = M796()
    Case 797: MC7 = M797()
    Case 798: MC7 = M798()
    Case 799: MC7 = M799()
    End Select
    Exit Function
EH:
    MC7 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC8(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 800: MC8 = M800()
    Case 801: MC8 = M801()
    Case 802: MC8 = M802()
    Case 803: MC8 = M803()
    Case 804: MC8 = M804()
    Case 805: MC8 = M805()
    Case 806: MC8 = M806()
    Case 807: MC8 = M807()
    Case 808: MC8 = M808()
    Case 809: MC8 = M809()
    Case 810: MC8 = M810()
    Case 811: MC8 = M811()
    Case 812: MC8 = M812()
    Case 813: MC8 = M813()
    Case 814: MC8 = M814()
    Case 815: MC8 = M815()
    Case 816: MC8 = M816()
    Case 817: MC8 = M817()
    Case 818: MC8 = M818()
    Case 819: MC8 = M819()
    Case 820: MC8 = M820()
    Case 821: MC8 = M821()
    Case 822: MC8 = M822()
    Case 823: MC8 = M823()
    Case 824: MC8 = M824()
    Case 825: MC8 = M825()
    Case 826: MC8 = M826()
    Case 827: MC8 = M827()
    Case 828: MC8 = M828()
    Case 829: MC8 = M829()
    Case 830: MC8 = M830()
    Case 831: MC8 = M831()
    Case 832: MC8 = M832()
    Case 833: MC8 = M833()
    Case 834: MC8 = M834()
    Case 835: MC8 = M835()
    Case 836: MC8 = M836()
    Case 837: MC8 = M837()
    Case 838: MC8 = M838()
    Case 839: MC8 = M839()
    Case 840: MC8 = M840()
    Case 841: MC8 = M841()
    Case 842: MC8 = M842()
    Case 843: MC8 = M843()
    Case 844: MC8 = M844()
    Case 845: MC8 = M845()
    Case 846: MC8 = M846()
    Case 847: MC8 = M847()
    Case 848: MC8 = M848()
    Case 849: MC8 = M849()
    Case 850: MC8 = M850()
    Case 851: MC8 = M851()
    Case 852: MC8 = M852()
    Case 853: MC8 = M853()
    Case 854: MC8 = M854()
    Case 855: MC8 = M855()
    Case 856: MC8 = M856()
    Case 857: MC8 = M857()
    Case 858: MC8 = M858()
    Case 859: MC8 = M859()
    Case 860: MC8 = M860()
    Case 861: MC8 = M861()
    Case 862: MC8 = M862()
    Case 863: MC8 = M863()
    Case 864: MC8 = M864()
    Case 865: MC8 = M865()
    Case 866: MC8 = M866()
    Case 867: MC8 = M867()
    Case 868: MC8 = M868()
    Case 869: MC8 = M869()
    Case 870: MC8 = M870()
    Case 871: MC8 = M871()
    Case 872: MC8 = M872()
    Case 873: MC8 = M873()
    Case 874: MC8 = M874()
    Case 875: MC8 = M875()
    Case 876: MC8 = M876()
    Case 877: MC8 = M877()
    Case 878: MC8 = M878()
    Case 879: MC8 = M879()
    Case 880: MC8 = M880()
    Case 881: MC8 = M881()
    Case 882: MC8 = M882()
    Case 883: MC8 = M883()
    Case 884: MC8 = M884()
    Case 885: MC8 = M885()
    Case 886: MC8 = M886()
    Case 887: MC8 = M887()
    Case 888: MC8 = M888()
    Case 889: MC8 = M889()
    Case 890: MC8 = M890()
    Case 891: MC8 = M891()
    Case 892: MC8 = M892()
    Case 893: MC8 = M893()
    Case 894: MC8 = M894()
    Case 895: MC8 = M895()
    Case 896: MC8 = M896()
    Case 897: MC8 = M897()
    Case 898: MC8 = M898()
    Case 899: MC8 = M899()
    End Select
    Exit Function
EH:
    MC8 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC9(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 900: MC9 = M900()
    Case 901: MC9 = M901()
    Case 902: MC9 = M902()
    Case 903: MC9 = M903()
    Case 904: MC9 = M904()
    Case 905: MC9 = M905()
    Case 906: MC9 = M906()
    Case 907: MC9 = M907()
    Case 908: MC9 = M908()
    Case 909: MC9 = M909()
    Case 910: MC9 = M910()
    Case 911: MC9 = M911()
    Case 912: MC9 = M912()
    Case 913: MC9 = M913()
    Case 914: MC9 = M914()
    Case 915: MC9 = M915()
    Case 916: MC9 = M916()
    Case 917: MC9 = M917()
    Case 918: MC9 = M918()
    Case 919: MC9 = M919()
    Case 920: MC9 = M920()
    Case 921: MC9 = M921()
    Case 922: MC9 = M922()
    Case 923: MC9 = M923()
    Case 924: MC9 = M924()
    Case 925: MC9 = M925()
    Case 926: MC9 = M926()
    Case 927: MC9 = M927()
    Case 928: MC9 = M928()
    Case 929: MC9 = M929()
    Case 930: MC9 = M930()
    Case 931: MC9 = M931()
    Case 932: MC9 = M932()
    Case 933: MC9 = M933()
    Case 934: MC9 = M934()
    Case 935: MC9 = M935()
    Case 936: MC9 = M936()
    Case 937: MC9 = M937()
    Case 938: MC9 = M938()
    Case 939: MC9 = M939()
    Case 940: MC9 = M940()
    Case 941: MC9 = M941()
    Case 942: MC9 = M942()
    Case 943: MC9 = M943()
    Case 944: MC9 = M944()
    Case 945: MC9 = M945()
    Case 946: MC9 = M946()
    Case 947: MC9 = M947()
    Case 948: MC9 = M948()
    Case 949: MC9 = M949()
    Case 950: MC9 = M950()
    Case 951: MC9 = M951()
    Case 952: MC9 = M952()
    Case 953: MC9 = M953()
    Case 954: MC9 = M954()
    Case 955: MC9 = M955()
    Case 956: MC9 = M956()
    Case 957: MC9 = M957()
    Case 958: MC9 = M958()
    Case 959: MC9 = M959()
    Case 960: MC9 = M960()
    Case 961: MC9 = M961()
    Case 962: MC9 = M962()
    Case 963: MC9 = M963()
    Case 964: MC9 = M964()
    Case 965: MC9 = M965()
    Case 966: MC9 = M966()
    Case 967: MC9 = M967()
    Case 968: MC9 = M968()
    Case 969: MC9 = M969()
    Case 970: MC9 = M970()
    Case 971: MC9 = M971()
    Case 972: MC9 = M972()
    Case 973: MC9 = M973()
    Case 974: MC9 = M974()
    Case 975: MC9 = M975()
    Case 976: MC9 = M976()
    Case 977: MC9 = M977()
    Case 978: MC9 = M978()
    Case 979: MC9 = M979()
    Case 980: MC9 = M980()
    Case 981: MC9 = M981()
    Case 982: MC9 = M982()
    Case 983: MC9 = M983()
    Case 984: MC9 = M984()
    Case 985: MC9 = M985()
    Case 986: MC9 = M986()
    Case 987: MC9 = M987()
    Case 988: MC9 = M988()
    Case 989: MC9 = M989()
    Case 990: MC9 = M990()
    Case 991: MC9 = M991()
    Case 992: MC9 = M992()
    Case 993: MC9 = M993()
    Case 994: MC9 = M994()
    Case 995: MC9 = M995()
    Case 996: MC9 = M996()
    Case 997: MC9 = M997()
    Case 998: MC9 = M998()
    Case 999: MC9 = M999()
    End Select
    Exit Function
EH:
    MC9 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M0() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "0")
    M0 = R
    Exit Function
EH:
    M0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "0.00")
    M1 = R
    Exit Function
EH:
    M1 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "#,##0")
    M2 = R
    Exit Function
EH:
    M2 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "#,##0.00")
    M3 = R
    Exit Function
EH:
    M3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "0.0%")
    M4 = R
    Exit Function
EH:
    M4 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M5() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "0.00E+00")
    M5 = R
    Exit Function
EH:
    M5 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M6() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "#.##")
    M6 = R
    Exit Function
EH:
    M6 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M7() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "000.000")
    M7 = R
    Exit Function
EH:
    M7 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M8() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "$#,##0.00;($#,##0.00)")
    M8 = R
    Exit Function
EH:
    M8 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M9() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "0;-0;Zero")
    M9 = R
    Exit Function
EH:
    M9 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M10() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "@")
    M10 = R
    Exit Function
EH:
    M10 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M11() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "General Number")
    M11 = R
    Exit Function
EH:
    M11 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M12() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "Currency")
    M12 = R
    Exit Function
EH:
    M12 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M13() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "Fixed")
    M13 = R
    Exit Function
EH:
    M13 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M14() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "Standard")
    M14 = R
    Exit Function
EH:
    M14 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M15() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "Percent")
    M15 = R
    Exit Function
EH:
    M15 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M16() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "Scientific")
    M16 = R
    Exit Function
EH:
    M16 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M17() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "Yes/No")
    M17 = R
    Exit Function
EH:
    M17 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M18() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "True/False")
    M18 = R
    Exit Function
EH:
    M18 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M19() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "On/Off")
    M19 = R
    Exit Function
EH:
    M19 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M20() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "General")
    M20 = R
    Exit Function
EH:
    M20 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M21() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "0.0##")
    M21 = R
    Exit Function
EH:
    M21 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M22() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "#,###")
    M22 = R
    Exit Function
EH:
    M22 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M23() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "\x0")
    M23 = R
    Exit Function
EH:
    M23 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M24() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "0 \u")
    M24 = R
    Exit Function
EH:
    M24 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M25() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, ">")
    M25 = R
    Exit Function
EH:
    M25 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M26() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "+0;-0")
    M26 = R
    Exit Function
EH:
    M26 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M27() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "#.0#E-0")
    M27 = R
    Exit Function
EH:
    M27 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M28() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0")
    M28 = R
    Exit Function
EH:
    M28 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M29() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0.00")
    M29 = R
    Exit Function
EH:
    M29 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M30() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "#,##0")
    M30 = R
    Exit Function
EH:
    M30 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M31() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "#,##0.00")
    M31 = R
    Exit Function
EH:
    M31 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M32() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0.0%")
    M32 = R
    Exit Function
EH:
    M32 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M33() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0.00E+00")
    M33 = R
    Exit Function
EH:
    M33 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M34() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "#.##")
    M34 = R
    Exit Function
EH:
    M34 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M35() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "000.000")
    M35 = R
    Exit Function
EH:
    M35 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M36() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "$#,##0.00;($#,##0.00)")
    M36 = R
    Exit Function
EH:
    M36 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M37() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0;-0;Zero")
    M37 = R
    Exit Function
EH:
    M37 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M38() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "@")
    M38 = R
    Exit Function
EH:
    M38 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M39() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "General Number")
    M39 = R
    Exit Function
EH:
    M39 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M40() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "Currency")
    M40 = R
    Exit Function
EH:
    M40 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M41() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "Fixed")
    M41 = R
    Exit Function
EH:
    M41 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M42() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "Standard")
    M42 = R
    Exit Function
EH:
    M42 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M43() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "Percent")
    M43 = R
    Exit Function
EH:
    M43 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M44() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "Scientific")
    M44 = R
    Exit Function
EH:
    M44 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M45() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "Yes/No")
    M45 = R
    Exit Function
EH:
    M45 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M46() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "True/False")
    M46 = R
    Exit Function
EH:
    M46 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M47() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "On/Off")
    M47 = R
    Exit Function
EH:
    M47 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M48() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "General")
    M48 = R
    Exit Function
EH:
    M48 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M49() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0.0##")
    M49 = R
    Exit Function
EH:
    M49 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M50() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "#,###")
    M50 = R
    Exit Function
EH:
    M50 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M51() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "\x0")
    M51 = R
    Exit Function
EH:
    M51 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M52() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0 \u")
    M52 = R
    Exit Function
EH:
    M52 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M53() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, ">")
    M53 = R
    Exit Function
EH:
    M53 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M54() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "+0;-0")
    M54 = R
    Exit Function
EH:
    M54 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M55() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "#.0#E-0")
    M55 = R
    Exit Function
EH:
    M55 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M56() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "0")
    M56 = R
    Exit Function
EH:
    M56 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M57() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "0.00")
    M57 = R
    Exit Function
EH:
    M57 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M58() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "#,##0")
    M58 = R
    Exit Function
EH:
    M58 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M59() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "#,##0.00")
    M59 = R
    Exit Function
EH:
    M59 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M60() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "0.0%")
    M60 = R
    Exit Function
EH:
    M60 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M61() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "0.00E+00")
    M61 = R
    Exit Function
EH:
    M61 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M62() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "#.##")
    M62 = R
    Exit Function
EH:
    M62 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M63() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "000.000")
    M63 = R
    Exit Function
EH:
    M63 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M64() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "$#,##0.00;($#,##0.00)")
    M64 = R
    Exit Function
EH:
    M64 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M65() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "0;-0;Zero")
    M65 = R
    Exit Function
EH:
    M65 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M66() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "@")
    M66 = R
    Exit Function
EH:
    M66 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M67() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "General Number")
    M67 = R
    Exit Function
EH:
    M67 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M68() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "Currency")
    M68 = R
    Exit Function
EH:
    M68 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M69() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "Fixed")
    M69 = R
    Exit Function
EH:
    M69 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M70() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "Standard")
    M70 = R
    Exit Function
EH:
    M70 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M71() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "Percent")
    M71 = R
    Exit Function
EH:
    M71 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M72() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "Scientific")
    M72 = R
    Exit Function
EH:
    M72 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M73() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "Yes/No")
    M73 = R
    Exit Function
EH:
    M73 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M74() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "True/False")
    M74 = R
    Exit Function
EH:
    M74 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M75() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "On/Off")
    M75 = R
    Exit Function
EH:
    M75 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M76() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "General")
    M76 = R
    Exit Function
EH:
    M76 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M77() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "0.0##")
    M77 = R
    Exit Function
EH:
    M77 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M78() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "#,###")
    M78 = R
    Exit Function
EH:
    M78 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M79() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "\x0")
    M79 = R
    Exit Function
EH:
    M79 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M80() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "0 \u")
    M80 = R
    Exit Function
EH:
    M80 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M81() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, ">")
    M81 = R
    Exit Function
EH:
    M81 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M82() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "+0;-0")
    M82 = R
    Exit Function
EH:
    M82 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M83() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1, "#.0#E-0")
    M83 = R
    Exit Function
EH:
    M83 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M84() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "0")
    M84 = R
    Exit Function
EH:
    M84 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M85() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "0.00")
    M85 = R
    Exit Function
EH:
    M85 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M86() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "#,##0")
    M86 = R
    Exit Function
EH:
    M86 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M87() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "#,##0.00")
    M87 = R
    Exit Function
EH:
    M87 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M88() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "0.0%")
    M88 = R
    Exit Function
EH:
    M88 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M89() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "0.00E+00")
    M89 = R
    Exit Function
EH:
    M89 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M90() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "#.##")
    M90 = R
    Exit Function
EH:
    M90 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M91() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "000.000")
    M91 = R
    Exit Function
EH:
    M91 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M92() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "$#,##0.00;($#,##0.00)")
    M92 = R
    Exit Function
EH:
    M92 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M93() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "0;-0;Zero")
    M93 = R
    Exit Function
EH:
    M93 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M94() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "@")
    M94 = R
    Exit Function
EH:
    M94 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M95() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "General Number")
    M95 = R
    Exit Function
EH:
    M95 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M96() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "Currency")
    M96 = R
    Exit Function
EH:
    M96 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M97() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "Fixed")
    M97 = R
    Exit Function
EH:
    M97 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M98() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "Standard")
    M98 = R
    Exit Function
EH:
    M98 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M99() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "Percent")
    M99 = R
    Exit Function
EH:
    M99 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M100() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "Scientific")
    M100 = R
    Exit Function
EH:
    M100 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M101() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "Yes/No")
    M101 = R
    Exit Function
EH:
    M101 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M102() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "True/False")
    M102 = R
    Exit Function
EH:
    M102 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M103() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "On/Off")
    M103 = R
    Exit Function
EH:
    M103 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M104() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "General")
    M104 = R
    Exit Function
EH:
    M104 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M105() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "0.0##")
    M105 = R
    Exit Function
EH:
    M105 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M106() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "#,###")
    M106 = R
    Exit Function
EH:
    M106 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M107() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "\x0")
    M107 = R
    Exit Function
EH:
    M107 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M108() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "0 \u")
    M108 = R
    Exit Function
EH:
    M108 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M109() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, ">")
    M109 = R
    Exit Function
EH:
    M109 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M110() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "+0;-0")
    M110 = R
    Exit Function
EH:
    M110 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M111() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "#.0#E-0")
    M111 = R
    Exit Function
EH:
    M111 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M112() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "0")
    M112 = R
    Exit Function
EH:
    M112 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M113() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "0.00")
    M113 = R
    Exit Function
EH:
    M113 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M114() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "#,##0")
    M114 = R
    Exit Function
EH:
    M114 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M115() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "#,##0.00")
    M115 = R
    Exit Function
EH:
    M115 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M116() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "0.0%")
    M116 = R
    Exit Function
EH:
    M116 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M117() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "0.00E+00")
    M117 = R
    Exit Function
EH:
    M117 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M118() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "#.##")
    M118 = R
    Exit Function
EH:
    M118 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M119() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "000.000")
    M119 = R
    Exit Function
EH:
    M119 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M120() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "$#,##0.00;($#,##0.00)")
    M120 = R
    Exit Function
EH:
    M120 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M121() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "0;-0;Zero")
    M121 = R
    Exit Function
EH:
    M121 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M122() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "@")
    M122 = R
    Exit Function
EH:
    M122 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M123() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "General Number")
    M123 = R
    Exit Function
EH:
    M123 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M124() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "Currency")
    M124 = R
    Exit Function
EH:
    M124 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M125() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "Fixed")
    M125 = R
    Exit Function
EH:
    M125 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M126() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "Standard")
    M126 = R
    Exit Function
EH:
    M126 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M127() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "Percent")
    M127 = R
    Exit Function
EH:
    M127 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M128() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "Scientific")
    M128 = R
    Exit Function
EH:
    M128 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M129() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "Yes/No")
    M129 = R
    Exit Function
EH:
    M129 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M130() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "True/False")
    M130 = R
    Exit Function
EH:
    M130 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M131() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "On/Off")
    M131 = R
    Exit Function
EH:
    M131 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M132() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "General")
    M132 = R
    Exit Function
EH:
    M132 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M133() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "0.0##")
    M133 = R
    Exit Function
EH:
    M133 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M134() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "#,###")
    M134 = R
    Exit Function
EH:
    M134 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M135() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "\x0")
    M135 = R
    Exit Function
EH:
    M135 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M136() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "0 \u")
    M136 = R
    Exit Function
EH:
    M136 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M137() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, ">")
    M137 = R
    Exit Function
EH:
    M137 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M138() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "+0;-0")
    M138 = R
    Exit Function
EH:
    M138 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M139() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-0.5, "#.0#E-0")
    M139 = R
    Exit Function
EH:
    M139 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M140() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "0")
    M140 = R
    Exit Function
EH:
    M140 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M141() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "0.00")
    M141 = R
    Exit Function
EH:
    M141 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M142() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "#,##0")
    M142 = R
    Exit Function
EH:
    M142 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M143() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "#,##0.00")
    M143 = R
    Exit Function
EH:
    M143 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M144() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "0.0%")
    M144 = R
    Exit Function
EH:
    M144 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M145() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "0.00E+00")
    M145 = R
    Exit Function
EH:
    M145 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M146() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "#.##")
    M146 = R
    Exit Function
EH:
    M146 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M147() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "000.000")
    M147 = R
    Exit Function
EH:
    M147 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M148() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "$#,##0.00;($#,##0.00)")
    M148 = R
    Exit Function
EH:
    M148 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M149() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "0;-0;Zero")
    M149 = R
    Exit Function
EH:
    M149 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M150() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "@")
    M150 = R
    Exit Function
EH:
    M150 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M151() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "General Number")
    M151 = R
    Exit Function
EH:
    M151 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M152() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "Currency")
    M152 = R
    Exit Function
EH:
    M152 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M153() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "Fixed")
    M153 = R
    Exit Function
EH:
    M153 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M154() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "Standard")
    M154 = R
    Exit Function
EH:
    M154 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M155() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "Percent")
    M155 = R
    Exit Function
EH:
    M155 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M156() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "Scientific")
    M156 = R
    Exit Function
EH:
    M156 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M157() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "Yes/No")
    M157 = R
    Exit Function
EH:
    M157 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M158() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "True/False")
    M158 = R
    Exit Function
EH:
    M158 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M159() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "On/Off")
    M159 = R
    Exit Function
EH:
    M159 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M160() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "General")
    M160 = R
    Exit Function
EH:
    M160 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M161() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "0.0##")
    M161 = R
    Exit Function
EH:
    M161 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M162() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "#,###")
    M162 = R
    Exit Function
EH:
    M162 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M163() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "\x0")
    M163 = R
    Exit Function
EH:
    M163 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M164() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "0 \u")
    M164 = R
    Exit Function
EH:
    M164 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M165() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, ">")
    M165 = R
    Exit Function
EH:
    M165 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M166() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "+0;-0")
    M166 = R
    Exit Function
EH:
    M166 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M167() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5678, "#.0#E-0")
    M167 = R
    Exit Function
EH:
    M167 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M168() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "0")
    M168 = R
    Exit Function
EH:
    M168 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M169() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "0.00")
    M169 = R
    Exit Function
EH:
    M169 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M170() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "#,##0")
    M170 = R
    Exit Function
EH:
    M170 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M171() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "#,##0.00")
    M171 = R
    Exit Function
EH:
    M171 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M172() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "0.0%")
    M172 = R
    Exit Function
EH:
    M172 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M173() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "0.00E+00")
    M173 = R
    Exit Function
EH:
    M173 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M174() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "#.##")
    M174 = R
    Exit Function
EH:
    M174 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M175() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "000.000")
    M175 = R
    Exit Function
EH:
    M175 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M176() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "$#,##0.00;($#,##0.00)")
    M176 = R
    Exit Function
EH:
    M176 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M177() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "0;-0;Zero")
    M177 = R
    Exit Function
EH:
    M177 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M178() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "@")
    M178 = R
    Exit Function
EH:
    M178 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M179() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "General Number")
    M179 = R
    Exit Function
EH:
    M179 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M180() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "Currency")
    M180 = R
    Exit Function
EH:
    M180 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M181() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "Fixed")
    M181 = R
    Exit Function
EH:
    M181 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M182() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "Standard")
    M182 = R
    Exit Function
EH:
    M182 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M183() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "Percent")
    M183 = R
    Exit Function
EH:
    M183 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M184() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "Scientific")
    M184 = R
    Exit Function
EH:
    M184 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M185() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "Yes/No")
    M185 = R
    Exit Function
EH:
    M185 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M186() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "True/False")
    M186 = R
    Exit Function
EH:
    M186 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M187() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "On/Off")
    M187 = R
    Exit Function
EH:
    M187 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M188() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "General")
    M188 = R
    Exit Function
EH:
    M188 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M189() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "0.0##")
    M189 = R
    Exit Function
EH:
    M189 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M190() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "#,###")
    M190 = R
    Exit Function
EH:
    M190 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M191() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "\x0")
    M191 = R
    Exit Function
EH:
    M191 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M192() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "0 \u")
    M192 = R
    Exit Function
EH:
    M192 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M193() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, ">")
    M193 = R
    Exit Function
EH:
    M193 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M194() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "+0;-0")
    M194 = R
    Exit Function
EH:
    M194 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M195() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1234.5678, "#.0#E-0")
    M195 = R
    Exit Function
EH:
    M195 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M196() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "0")
    M196 = R
    Exit Function
EH:
    M196 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M197() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "0.00")
    M197 = R
    Exit Function
EH:
    M197 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M198() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "#,##0")
    M198 = R
    Exit Function
EH:
    M198 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M199() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "#,##0.00")
    M199 = R
    Exit Function
EH:
    M199 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M200() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "0.0%")
    M200 = R
    Exit Function
EH:
    M200 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M201() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "0.00E+00")
    M201 = R
    Exit Function
EH:
    M201 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M202() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "#.##")
    M202 = R
    Exit Function
EH:
    M202 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M203() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "000.000")
    M203 = R
    Exit Function
EH:
    M203 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M204() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "$#,##0.00;($#,##0.00)")
    M204 = R
    Exit Function
EH:
    M204 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M205() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "0;-0;Zero")
    M205 = R
    Exit Function
EH:
    M205 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M206() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "@")
    M206 = R
    Exit Function
EH:
    M206 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M207() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "General Number")
    M207 = R
    Exit Function
EH:
    M207 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M208() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "Currency")
    M208 = R
    Exit Function
EH:
    M208 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M209() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "Fixed")
    M209 = R
    Exit Function
EH:
    M209 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M210() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "Standard")
    M210 = R
    Exit Function
EH:
    M210 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M211() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "Percent")
    M211 = R
    Exit Function
EH:
    M211 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M212() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "Scientific")
    M212 = R
    Exit Function
EH:
    M212 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M213() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "Yes/No")
    M213 = R
    Exit Function
EH:
    M213 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M214() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "True/False")
    M214 = R
    Exit Function
EH:
    M214 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M215() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "On/Off")
    M215 = R
    Exit Function
EH:
    M215 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M216() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "General")
    M216 = R
    Exit Function
EH:
    M216 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M217() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "0.0##")
    M217 = R
    Exit Function
EH:
    M217 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M218() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "#,###")
    M218 = R
    Exit Function
EH:
    M218 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M219() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "\x0")
    M219 = R
    Exit Function
EH:
    M219 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M220() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "0 \u")
    M220 = R
    Exit Function
EH:
    M220 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M221() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, ">")
    M221 = R
    Exit Function
EH:
    M221 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M222() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "+0;-0")
    M222 = R
    Exit Function
EH:
    M222 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M223() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.005, "#.0#E-0")
    M223 = R
    Exit Function
EH:
    M223 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M224() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "0")
    M224 = R
    Exit Function
EH:
    M224 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M225() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "0.00")
    M225 = R
    Exit Function
EH:
    M225 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M226() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "#,##0")
    M226 = R
    Exit Function
EH:
    M226 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M227() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "#,##0.00")
    M227 = R
    Exit Function
EH:
    M227 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M228() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "0.0%")
    M228 = R
    Exit Function
EH:
    M228 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M229() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "0.00E+00")
    M229 = R
    Exit Function
EH:
    M229 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M230() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "#.##")
    M230 = R
    Exit Function
EH:
    M230 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M231() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "000.000")
    M231 = R
    Exit Function
EH:
    M231 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M232() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "$#,##0.00;($#,##0.00)")
    M232 = R
    Exit Function
EH:
    M232 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M233() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "0;-0;Zero")
    M233 = R
    Exit Function
EH:
    M233 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M234() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "@")
    M234 = R
    Exit Function
EH:
    M234 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M235() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "General Number")
    M235 = R
    Exit Function
EH:
    M235 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M236() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "Currency")
    M236 = R
    Exit Function
EH:
    M236 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M237() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "Fixed")
    M237 = R
    Exit Function
EH:
    M237 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M238() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "Standard")
    M238 = R
    Exit Function
EH:
    M238 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M239() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "Percent")
    M239 = R
    Exit Function
EH:
    M239 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M240() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "Scientific")
    M240 = R
    Exit Function
EH:
    M240 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M241() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "Yes/No")
    M241 = R
    Exit Function
EH:
    M241 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M242() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "True/False")
    M242 = R
    Exit Function
EH:
    M242 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M243() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "On/Off")
    M243 = R
    Exit Function
EH:
    M243 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M244() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "General")
    M244 = R
    Exit Function
EH:
    M244 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M245() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "0.0##")
    M245 = R
    Exit Function
EH:
    M245 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M246() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "#,###")
    M246 = R
    Exit Function
EH:
    M246 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M247() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "\x0")
    M247 = R
    Exit Function
EH:
    M247 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M248() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "0 \u")
    M248 = R
    Exit Function
EH:
    M248 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M249() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, ">")
    M249 = R
    Exit Function
EH:
    M249 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M250() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "+0;-0")
    M250 = R
    Exit Function
EH:
    M250 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M251() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.995, "#.0#E-0")
    M251 = R
    Exit Function
EH:
    M251 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M252() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "0")
    M252 = R
    Exit Function
EH:
    M252 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M253() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "0.00")
    M253 = R
    Exit Function
EH:
    M253 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M254() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "#,##0")
    M254 = R
    Exit Function
EH:
    M254 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M255() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "#,##0.00")
    M255 = R
    Exit Function
EH:
    M255 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M256() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "0.0%")
    M256 = R
    Exit Function
EH:
    M256 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M257() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "0.00E+00")
    M257 = R
    Exit Function
EH:
    M257 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M258() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "#.##")
    M258 = R
    Exit Function
EH:
    M258 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M259() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "000.000")
    M259 = R
    Exit Function
EH:
    M259 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M260() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "$#,##0.00;($#,##0.00)")
    M260 = R
    Exit Function
EH:
    M260 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M261() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "0;-0;Zero")
    M261 = R
    Exit Function
EH:
    M261 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M262() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "@")
    M262 = R
    Exit Function
EH:
    M262 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M263() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "General Number")
    M263 = R
    Exit Function
EH:
    M263 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M264() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "Currency")
    M264 = R
    Exit Function
EH:
    M264 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M265() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "Fixed")
    M265 = R
    Exit Function
EH:
    M265 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M266() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "Standard")
    M266 = R
    Exit Function
EH:
    M266 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M267() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "Percent")
    M267 = R
    Exit Function
EH:
    M267 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M268() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "Scientific")
    M268 = R
    Exit Function
EH:
    M268 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M269() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "Yes/No")
    M269 = R
    Exit Function
EH:
    M269 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M270() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "True/False")
    M270 = R
    Exit Function
EH:
    M270 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M271() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "On/Off")
    M271 = R
    Exit Function
EH:
    M271 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M272() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "General")
    M272 = R
    Exit Function
EH:
    M272 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M273() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "0.0##")
    M273 = R
    Exit Function
EH:
    M273 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M274() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "#,###")
    M274 = R
    Exit Function
EH:
    M274 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M275() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "\x0")
    M275 = R
    Exit Function
EH:
    M275 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M276() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "0 \u")
    M276 = R
    Exit Function
EH:
    M276 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M277() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, ">")
    M277 = R
    Exit Function
EH:
    M277 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M278() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "+0;-0")
    M278 = R
    Exit Function
EH:
    M278 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M279() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E10, "#.0#E-0")
    M279 = R
    Exit Function
EH:
    M279 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M280() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "0")
    M280 = R
    Exit Function
EH:
    M280 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M281() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "0.00")
    M281 = R
    Exit Function
EH:
    M281 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M282() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "#,##0")
    M282 = R
    Exit Function
EH:
    M282 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M283() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "#,##0.00")
    M283 = R
    Exit Function
EH:
    M283 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M284() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "0.0%")
    M284 = R
    Exit Function
EH:
    M284 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M285() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "0.00E+00")
    M285 = R
    Exit Function
EH:
    M285 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M286() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "#.##")
    M286 = R
    Exit Function
EH:
    M286 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M287() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "000.000")
    M287 = R
    Exit Function
EH:
    M287 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M288() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "$#,##0.00;($#,##0.00)")
    M288 = R
    Exit Function
EH:
    M288 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M289() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "0;-0;Zero")
    M289 = R
    Exit Function
EH:
    M289 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M290() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "@")
    M290 = R
    Exit Function
EH:
    M290 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M291() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "General Number")
    M291 = R
    Exit Function
EH:
    M291 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M292() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "Currency")
    M292 = R
    Exit Function
EH:
    M292 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M293() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "Fixed")
    M293 = R
    Exit Function
EH:
    M293 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M294() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "Standard")
    M294 = R
    Exit Function
EH:
    M294 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M295() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "Percent")
    M295 = R
    Exit Function
EH:
    M295 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M296() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "Scientific")
    M296 = R
    Exit Function
EH:
    M296 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M297() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "Yes/No")
    M297 = R
    Exit Function
EH:
    M297 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M298() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "True/False")
    M298 = R
    Exit Function
EH:
    M298 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M299() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "On/Off")
    M299 = R
    Exit Function
EH:
    M299 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M300() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "General")
    M300 = R
    Exit Function
EH:
    M300 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M301() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "0.0##")
    M301 = R
    Exit Function
EH:
    M301 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M302() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "#,###")
    M302 = R
    Exit Function
EH:
    M302 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M303() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "\x0")
    M303 = R
    Exit Function
EH:
    M303 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M304() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "0 \u")
    M304 = R
    Exit Function
EH:
    M304 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M305() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, ">")
    M305 = R
    Exit Function
EH:
    M305 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M306() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "+0;-0")
    M306 = R
    Exit Function
EH:
    M306 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M307() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234567.891, "#.0#E-0")
    M307 = R
    Exit Function
EH:
    M307 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M308() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "0")
    M308 = R
    Exit Function
EH:
    M308 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M309() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "0.00")
    M309 = R
    Exit Function
EH:
    M309 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M310() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "#,##0")
    M310 = R
    Exit Function
EH:
    M310 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M311() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "#,##0.00")
    M311 = R
    Exit Function
EH:
    M311 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M312() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "0.0%")
    M312 = R
    Exit Function
EH:
    M312 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M313() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "0.00E+00")
    M313 = R
    Exit Function
EH:
    M313 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M314() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "#.##")
    M314 = R
    Exit Function
EH:
    M314 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M315() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "000.000")
    M315 = R
    Exit Function
EH:
    M315 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M316() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "$#,##0.00;($#,##0.00)")
    M316 = R
    Exit Function
EH:
    M316 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M317() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "0;-0;Zero")
    M317 = R
    Exit Function
EH:
    M317 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M318() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "@")
    M318 = R
    Exit Function
EH:
    M318 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M319() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "General Number")
    M319 = R
    Exit Function
EH:
    M319 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M320() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "Currency")
    M320 = R
    Exit Function
EH:
    M320 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M321() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "Fixed")
    M321 = R
    Exit Function
EH:
    M321 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M322() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "Standard")
    M322 = R
    Exit Function
EH:
    M322 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M323() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "Percent")
    M323 = R
    Exit Function
EH:
    M323 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M324() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "Scientific")
    M324 = R
    Exit Function
EH:
    M324 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M325() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "Yes/No")
    M325 = R
    Exit Function
EH:
    M325 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M326() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "True/False")
    M326 = R
    Exit Function
EH:
    M326 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M327() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "On/Off")
    M327 = R
    Exit Function
EH:
    M327 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M328() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "General")
    M328 = R
    Exit Function
EH:
    M328 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M329() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "0.0##")
    M329 = R
    Exit Function
EH:
    M329 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M330() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "#,###")
    M330 = R
    Exit Function
EH:
    M330 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M331() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "\x0")
    M331 = R
    Exit Function
EH:
    M331 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M332() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "0 \u")
    M332 = R
    Exit Function
EH:
    M332 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M333() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, ">")
    M333 = R
    Exit Function
EH:
    M333 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M334() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "+0;-0")
    M334 = R
    Exit Function
EH:
    M334 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M335() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "#.0#E-0")
    M335 = R
    Exit Function
EH:
    M335 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M336() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "0")
    M336 = R
    Exit Function
EH:
    M336 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M337() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "0.00")
    M337 = R
    Exit Function
EH:
    M337 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M338() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "#,##0")
    M338 = R
    Exit Function
EH:
    M338 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M339() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "#,##0.00")
    M339 = R
    Exit Function
EH:
    M339 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M340() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "0.0%")
    M340 = R
    Exit Function
EH:
    M340 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M341() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "0.00E+00")
    M341 = R
    Exit Function
EH:
    M341 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M342() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "#.##")
    M342 = R
    Exit Function
EH:
    M342 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M343() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "000.000")
    M343 = R
    Exit Function
EH:
    M343 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M344() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "$#,##0.00;($#,##0.00)")
    M344 = R
    Exit Function
EH:
    M344 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M345() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "0;-0;Zero")
    M345 = R
    Exit Function
EH:
    M345 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M346() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "@")
    M346 = R
    Exit Function
EH:
    M346 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M347() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "General Number")
    M347 = R
    Exit Function
EH:
    M347 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M348() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "Currency")
    M348 = R
    Exit Function
EH:
    M348 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M349() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "Fixed")
    M349 = R
    Exit Function
EH:
    M349 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M350() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "Standard")
    M350 = R
    Exit Function
EH:
    M350 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M351() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "Percent")
    M351 = R
    Exit Function
EH:
    M351 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M352() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "Scientific")
    M352 = R
    Exit Function
EH:
    M352 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M353() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "Yes/No")
    M353 = R
    Exit Function
EH:
    M353 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M354() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "True/False")
    M354 = R
    Exit Function
EH:
    M354 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M355() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "On/Off")
    M355 = R
    Exit Function
EH:
    M355 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M356() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "General")
    M356 = R
    Exit Function
EH:
    M356 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M357() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "0.0##")
    M357 = R
    Exit Function
EH:
    M357 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M358() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "#,###")
    M358 = R
    Exit Function
EH:
    M358 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M359() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "\x0")
    M359 = R
    Exit Function
EH:
    M359 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M360() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "0 \u")
    M360 = R
    Exit Function
EH:
    M360 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M361() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, ">")
    M361 = R
    Exit Function
EH:
    M361 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M362() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "+0;-0")
    M362 = R
    Exit Function
EH:
    M362 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M363() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12, "#.0#E-0")
    M363 = R
    Exit Function
EH:
    M363 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M364() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "0")
    M364 = R
    Exit Function
EH:
    M364 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M365() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "0.00")
    M365 = R
    Exit Function
EH:
    M365 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M366() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "#,##0")
    M366 = R
    Exit Function
EH:
    M366 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M367() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "#,##0.00")
    M367 = R
    Exit Function
EH:
    M367 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M368() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "0.0%")
    M368 = R
    Exit Function
EH:
    M368 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M369() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "0.00E+00")
    M369 = R
    Exit Function
EH:
    M369 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M370() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "#.##")
    M370 = R
    Exit Function
EH:
    M370 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M371() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "000.000")
    M371 = R
    Exit Function
EH:
    M371 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M372() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "$#,##0.00;($#,##0.00)")
    M372 = R
    Exit Function
EH:
    M372 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M373() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "0;-0;Zero")
    M373 = R
    Exit Function
EH:
    M373 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M374() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "@")
    M374 = R
    Exit Function
EH:
    M374 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M375() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "General Number")
    M375 = R
    Exit Function
EH:
    M375 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M376() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "Currency")
    M376 = R
    Exit Function
EH:
    M376 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M377() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "Fixed")
    M377 = R
    Exit Function
EH:
    M377 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M378() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "Standard")
    M378 = R
    Exit Function
EH:
    M378 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M379() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "Percent")
    M379 = R
    Exit Function
EH:
    M379 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M380() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "Scientific")
    M380 = R
    Exit Function
EH:
    M380 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M381() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "Yes/No")
    M381 = R
    Exit Function
EH:
    M381 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M382() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "True/False")
    M382 = R
    Exit Function
EH:
    M382 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M383() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "On/Off")
    M383 = R
    Exit Function
EH:
    M383 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M384() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "General")
    M384 = R
    Exit Function
EH:
    M384 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M385() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "0.0##")
    M385 = R
    Exit Function
EH:
    M385 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M386() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "#,###")
    M386 = R
    Exit Function
EH:
    M386 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M387() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "\x0")
    M387 = R
    Exit Function
EH:
    M387 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M388() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "0 \u")
    M388 = R
    Exit Function
EH:
    M388 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M389() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, ">")
    M389 = R
    Exit Function
EH:
    M389 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M390() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "+0;-0")
    M390 = R
    Exit Function
EH:
    M390 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M391() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(100, "#.0#E-0")
    M391 = R
    Exit Function
EH:
    M391 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M392() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "0")
    M392 = R
    Exit Function
EH:
    M392 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M393() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "0.00")
    M393 = R
    Exit Function
EH:
    M393 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M394() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "#,##0")
    M394 = R
    Exit Function
EH:
    M394 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M395() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "#,##0.00")
    M395 = R
    Exit Function
EH:
    M395 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M396() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "0.0%")
    M396 = R
    Exit Function
EH:
    M396 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M397() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "0.00E+00")
    M397 = R
    Exit Function
EH:
    M397 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M398() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "#.##")
    M398 = R
    Exit Function
EH:
    M398 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M399() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "000.000")
    M399 = R
    Exit Function
EH:
    M399 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M400() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "$#,##0.00;($#,##0.00)")
    M400 = R
    Exit Function
EH:
    M400 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M401() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "0;-0;Zero")
    M401 = R
    Exit Function
EH:
    M401 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M402() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "@")
    M402 = R
    Exit Function
EH:
    M402 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M403() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "General Number")
    M403 = R
    Exit Function
EH:
    M403 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M404() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "Currency")
    M404 = R
    Exit Function
EH:
    M404 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M405() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "Fixed")
    M405 = R
    Exit Function
EH:
    M405 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M406() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "Standard")
    M406 = R
    Exit Function
EH:
    M406 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M407() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "Percent")
    M407 = R
    Exit Function
EH:
    M407 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M408() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "Scientific")
    M408 = R
    Exit Function
EH:
    M408 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M409() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "Yes/No")
    M409 = R
    Exit Function
EH:
    M409 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M410() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "True/False")
    M410 = R
    Exit Function
EH:
    M410 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M411() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "On/Off")
    M411 = R
    Exit Function
EH:
    M411 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M412() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "General")
    M412 = R
    Exit Function
EH:
    M412 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M413() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "0.0##")
    M413 = R
    Exit Function
EH:
    M413 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M414() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "#,###")
    M414 = R
    Exit Function
EH:
    M414 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M415() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "\x0")
    M415 = R
    Exit Function
EH:
    M415 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M416() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "0 \u")
    M416 = R
    Exit Function
EH:
    M416 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M417() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, ">")
    M417 = R
    Exit Function
EH:
    M417 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M418() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "+0;-0")
    M418 = R
    Exit Function
EH:
    M418 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M419() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E-10, "#.0#E-0")
    M419 = R
    Exit Function
EH:
    M419 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M420() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(0) & "|" & FormatNumber(0, 0) & "|" & FormatNumber(0, 3, vbTrue) & "|" & FormatNumber(0, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatNumber(0, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M420 = R
    Exit Function
EH:
    M420 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M421() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(0) & "|" & FormatCurrency(0, 0) & "|" & FormatCurrency(0, 3, vbTrue) & "|" & FormatCurrency(0, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatCurrency(0, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M421 = R
    Exit Function
EH:
    M421 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M422() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(0) & "|" & FormatPercent(0, 0) & "|" & FormatPercent(0, 3, vbTrue) & "|" & FormatPercent(0, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatPercent(0, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M422 = R
    Exit Function
EH:
    M422 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M423() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(1) & "|" & FormatNumber(1, 0) & "|" & FormatNumber(1, 3, vbTrue) & "|" & FormatNumber(1, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatNumber(1, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M423 = R
    Exit Function
EH:
    M423 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M424() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(1) & "|" & FormatCurrency(1, 0) & "|" & FormatCurrency(1, 3, vbTrue) & "|" & FormatCurrency(1, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatCurrency(1, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M424 = R
    Exit Function
EH:
    M424 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M425() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(1) & "|" & FormatPercent(1, 0) & "|" & FormatPercent(1, 3, vbTrue) & "|" & FormatPercent(1, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatPercent(1, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M425 = R
    Exit Function
EH:
    M425 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M426() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(-1) & "|" & FormatNumber(-1, 0) & "|" & FormatNumber(-1, 3, vbTrue) & "|" & FormatNumber(-1, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatNumber(-1, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M426 = R
    Exit Function
EH:
    M426 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M427() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(-1) & "|" & FormatCurrency(-1, 0) & "|" & FormatCurrency(-1, 3, vbTrue) & "|" & FormatCurrency(-1, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatCurrency(-1, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M427 = R
    Exit Function
EH:
    M427 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M428() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(-1) & "|" & FormatPercent(-1, 0) & "|" & FormatPercent(-1, 3, vbTrue) & "|" & FormatPercent(-1, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatPercent(-1, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M428 = R
    Exit Function
EH:
    M428 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M429() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(0.5) & "|" & FormatNumber(0.5, 0) & "|" & FormatNumber(0.5, 3, vbTrue) & "|" & FormatNumber(0.5, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatNumber(0.5, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M429 = R
    Exit Function
EH:
    M429 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M430() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(0.5) & "|" & FormatCurrency(0.5, 0) & "|" & FormatCurrency(0.5, 3, vbTrue) & "|" & FormatCurrency(0.5, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatCurrency(0.5, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M430 = R
    Exit Function
EH:
    M430 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M431() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(0.5) & "|" & FormatPercent(0.5, 0) & "|" & FormatPercent(0.5, 3, vbTrue) & "|" & FormatPercent(0.5, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatPercent(0.5, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M431 = R
    Exit Function
EH:
    M431 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M432() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(-0.5) & "|" & FormatNumber(-0.5, 0) & "|" & FormatNumber(-0.5, 3, vbTrue) & "|" & FormatNumber(-0.5, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatNumber(-0.5, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M432 = R
    Exit Function
EH:
    M432 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M433() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(-0.5) & "|" & FormatCurrency(-0.5, 0) & "|" & FormatCurrency(-0.5, 3, vbTrue) & "|" & FormatCurrency(-0.5, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatCurrency(-0.5, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M433 = R
    Exit Function
EH:
    M433 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M434() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(-0.5) & "|" & FormatPercent(-0.5, 0) & "|" & FormatPercent(-0.5, 3, vbTrue) & "|" & FormatPercent(-0.5, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatPercent(-0.5, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M434 = R
    Exit Function
EH:
    M434 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M435() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(1234.5678) & "|" & FormatNumber(1234.5678, 0) & "|" & FormatNumber(1234.5678, 3, vbTrue) & "|" & FormatNumber(1234.5678, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatNumber(1234.5678, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M435 = R
    Exit Function
EH:
    M435 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M436() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(1234.5678) & "|" & FormatCurrency(1234.5678, 0) & "|" & FormatCurrency(1234.5678, 3, vbTrue) & "|" & FormatCurrency(1234.5678, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatCurrency(1234.5678, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M436 = R
    Exit Function
EH:
    M436 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M437() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(1234.5678) & "|" & FormatPercent(1234.5678, 0) & "|" & FormatPercent(1234.5678, 3, vbTrue) & "|" & FormatPercent(1234.5678, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatPercent(1234.5678, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M437 = R
    Exit Function
EH:
    M437 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M438() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(-1234.5678) & "|" & FormatNumber(-1234.5678, 0) & "|" & FormatNumber(-1234.5678, 3, vbTrue) & "|" & FormatNumber(-1234.5678, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatNumber(-1234.5678, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M438 = R
    Exit Function
EH:
    M438 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M439() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(-1234.5678) & "|" & FormatCurrency(-1234.5678, 0) & "|" & FormatCurrency(-1234.5678, 3, vbTrue) & "|" & FormatCurrency(-1234.5678, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatCurrency(-1234.5678, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M439 = R
    Exit Function
EH:
    M439 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M440() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(-1234.5678) & "|" & FormatPercent(-1234.5678, 0) & "|" & FormatPercent(-1234.5678, 3, vbTrue) & "|" & FormatPercent(-1234.5678, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatPercent(-1234.5678, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M440 = R
    Exit Function
EH:
    M440 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M441() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(0.005) & "|" & FormatNumber(0.005, 0) & "|" & FormatNumber(0.005, 3, vbTrue) & "|" & FormatNumber(0.005, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatNumber(0.005, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M441 = R
    Exit Function
EH:
    M441 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M442() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(0.005) & "|" & FormatCurrency(0.005, 0) & "|" & FormatCurrency(0.005, 3, vbTrue) & "|" & FormatCurrency(0.005, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatCurrency(0.005, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M442 = R
    Exit Function
EH:
    M442 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M443() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(0.005) & "|" & FormatPercent(0.005, 0) & "|" & FormatPercent(0.005, 3, vbTrue) & "|" & FormatPercent(0.005, 2, vbFalse, vbTrue, vbFalse) & "|" & FormatPercent(0.005, -1, vbUseDefault, vbUseDefault, vbUseDefault)
    M443 = R
    Exit Function
EH:
    M443 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M444() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(Null)
    M444 = R
    Exit Function
EH:
    M444 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M445() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber("abc")
    M445 = R
    Exit Function
EH:
    M445 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M446() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber("")
    M446 = R
    Exit Function
EH:
    M446 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M447() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(Empty)
    M447 = R
    Exit Function
EH:
    M447 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M448() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(1, 100)
    M448 = R
    Exit Function
EH:
    M448 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M449() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(1, -2)
    M449 = R
    Exit Function
EH:
    M449 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M450() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(1, 99)
    M450 = R
    Exit Function
EH:
    M450 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M451() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(0.256, 1, vbTrue, vbTrue, vbTrue)
    M451 = R
    Exit Function
EH:
    M451 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M452() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(-5, 2, vbTrue, vbTrue)
    M452 = R
    Exit Function
EH:
    M452 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M453() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(-5, 2, vbFalse, vbFalse)
    M453 = R
    Exit Function
EH:
    M453 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M454() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(-0.5, 2, vbFalse, vbTrue)
    M454 = R
    Exit Function
EH:
    M454 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M455() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(-0.5, 2, vbTrue, vbTrue)
    M455 = R
    Exit Function
EH:
    M455 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M456() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(1234567, 2, , , vbTrue)
    M456 = R
    Exit Function
EH:
    M456 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M457() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(1234567, 2, , , vbFalse)
    M457 = R
    Exit Function
EH:
    M457 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M458() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(CDate(36526), 1)
    M458 = R
    Exit Function
EH:
    M458 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M459() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber(True, 1)
    M459 = R
    Exit Function
EH:
    M459 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M460() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber("1,234.5", 1)
    M460 = R
    Exit Function
EH:
    M460 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M461() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatNumber("12abc", 1)
    M461 = R
    Exit Function
EH:
    M461 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M462() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatPercent(Null)
    M462 = R
    Exit Function
EH:
    M462 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M463() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency(Null)
    M463 = R
    Exit Function
EH:
    M463 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M464() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatCurrency("$5")
    M464 = R
    Exit Function
EH:
    M464 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M465() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#1/1/2000#) & "|" & FormatDateTime(#1/1/2000#, vbGeneralDate) & "|" & FormatDateTime(#1/1/2000#, vbLongDate) & "|" & FormatDateTime(#1/1/2000#, vbShortDate) & "|" & FormatDateTime(#1/1/2000#, vbLongTime) & "|" & FormatDateTime(#1/1/2000#, vbShortTime)
    M465 = R
    Exit Function
EH:
    M465 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M466() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#12/31/1999 11:59:59 PM#) & "|" & FormatDateTime(#12/31/1999 11:59:59 PM#, vbGeneralDate) & "|" & FormatDateTime(#12/31/1999 11:59:59 PM#, vbLongDate) & "|" & FormatDateTime(#12/31/1999 11:59:59 PM#, vbShortDate) & "|" & FormatDateTime(#12/31/1999 11:59:59 PM#, vbLongTime) & "|" & FormatDateTime(#12/31/1999 11:59:59 PM#, vbShortTime)
    M466 = R
    Exit Function
EH:
    M466 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M467() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#3/5/2001 1:02:03 AM#) & "|" & FormatDateTime(#3/5/2001 1:02:03 AM#, vbGeneralDate) & "|" & FormatDateTime(#3/5/2001 1:02:03 AM#, vbLongDate) & "|" & FormatDateTime(#3/5/2001 1:02:03 AM#, vbShortDate) & "|" & FormatDateTime(#3/5/2001 1:02:03 AM#, vbLongTime) & "|" & FormatDateTime(#3/5/2001 1:02:03 AM#, vbShortTime)
    M467 = R
    Exit Function
EH:
    M467 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M468() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#2/29/2000#) & "|" & FormatDateTime(#2/29/2000#, vbGeneralDate) & "|" & FormatDateTime(#2/29/2000#, vbLongDate) & "|" & FormatDateTime(#2/29/2000#, vbShortDate) & "|" & FormatDateTime(#2/29/2000#, vbLongTime) & "|" & FormatDateTime(#2/29/2000#, vbShortTime)
    M468 = R
    Exit Function
EH:
    M468 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M469() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#1/1/1900#) & "|" & FormatDateTime(#1/1/1900#, vbGeneralDate) & "|" & FormatDateTime(#1/1/1900#, vbLongDate) & "|" & FormatDateTime(#1/1/1900#, vbShortDate) & "|" & FormatDateTime(#1/1/1900#, vbLongTime) & "|" & FormatDateTime(#1/1/1900#, vbShortTime)
    M469 = R
    Exit Function
EH:
    M469 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M470() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#12/30/1899#) & "|" & FormatDateTime(#12/30/1899#, vbGeneralDate) & "|" & FormatDateTime(#12/30/1899#, vbLongDate) & "|" & FormatDateTime(#12/30/1899#, vbShortDate) & "|" & FormatDateTime(#12/30/1899#, vbLongTime) & "|" & FormatDateTime(#12/30/1899#, vbShortTime)
    M470 = R
    Exit Function
EH:
    M470 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M471() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#6/15/2022 12:00:00 PM#) & "|" & FormatDateTime(#6/15/2022 12:00:00 PM#, vbGeneralDate) & "|" & FormatDateTime(#6/15/2022 12:00:00 PM#, vbLongDate) & "|" & FormatDateTime(#6/15/2022 12:00:00 PM#, vbShortDate) & "|" & FormatDateTime(#6/15/2022 12:00:00 PM#, vbLongTime) & "|" & FormatDateTime(#6/15/2022 12:00:00 PM#, vbShortTime)
    M471 = R
    Exit Function
EH:
    M471 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M472() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#1/1/100#) & "|" & FormatDateTime(#1/1/100#, vbGeneralDate) & "|" & FormatDateTime(#1/1/100#, vbLongDate) & "|" & FormatDateTime(#1/1/100#, vbShortDate) & "|" & FormatDateTime(#1/1/100#, vbLongTime) & "|" & FormatDateTime(#1/1/100#, vbShortTime)
    M472 = R
    Exit Function
EH:
    M472 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M473() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(#12/31/9999#) & "|" & FormatDateTime(#12/31/9999#, vbGeneralDate) & "|" & FormatDateTime(#12/31/9999#, vbLongDate) & "|" & FormatDateTime(#12/31/9999#, vbShortDate) & "|" & FormatDateTime(#12/31/9999#, vbLongTime) & "|" & FormatDateTime(#12/31/9999#, vbShortTime)
    M473 = R
    Exit Function
EH:
    M473 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M474() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(CDate(0.25)) & "|" & FormatDateTime(CDate(0.25), vbGeneralDate) & "|" & FormatDateTime(CDate(0.25), vbLongDate) & "|" & FormatDateTime(CDate(0.25), vbShortDate) & "|" & FormatDateTime(CDate(0.25), vbLongTime) & "|" & FormatDateTime(CDate(0.25), vbShortTime)
    M474 = R
    Exit Function
EH:
    M474 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M475() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(CDate(-1.5)) & "|" & FormatDateTime(CDate(-1.5), vbGeneralDate) & "|" & FormatDateTime(CDate(-1.5), vbLongDate) & "|" & FormatDateTime(CDate(-1.5), vbShortDate) & "|" & FormatDateTime(CDate(-1.5), vbLongTime) & "|" & FormatDateTime(CDate(-1.5), vbShortTime)
    M475 = R
    Exit Function
EH:
    M475 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M476() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(Null)
    M476 = R
    Exit Function
EH:
    M476 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M477() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime("abc")
    M477 = R
    Exit Function
EH:
    M477 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M478() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(1, 5)
    M478 = R
    Exit Function
EH:
    M478 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M479() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(1, -1)
    M479 = R
    Exit Function
EH:
    M479 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M480() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(1.5, 3)
    M480 = R
    Exit Function
EH:
    M480 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M481() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime("1/2/2003 4:05", 0)
    M481 = R
    Exit Function
EH:
    M481 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M482() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime("13:45", 4)
    M482 = R
    Exit Function
EH:
    M482 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M483() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(Empty)
    M483 = R
    Exit Function
EH:
    M483 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M484() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime(True)
    M484 = R
    Exit Function
EH:
    M484 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M485() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FormatDateTime("")
    M485 = R
    Exit Function
EH:
    M485 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M486() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "d")
    M486 = R
    Exit Function
EH:
    M486 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M487() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "dd")
    M487 = R
    Exit Function
EH:
    M487 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M488() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "ddd")
    M488 = R
    Exit Function
EH:
    M488 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M489() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "dddd")
    M489 = R
    Exit Function
EH:
    M489 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M490() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "ddddd")
    M490 = R
    Exit Function
EH:
    M490 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M491() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "dddddd")
    M491 = R
    Exit Function
EH:
    M491 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M492() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "m")
    M492 = R
    Exit Function
EH:
    M492 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M493() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "mm")
    M493 = R
    Exit Function
EH:
    M493 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M494() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "mmm")
    M494 = R
    Exit Function
EH:
    M494 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M495() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "mmmm")
    M495 = R
    Exit Function
EH:
    M495 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M496() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "yy")
    M496 = R
    Exit Function
EH:
    M496 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M497() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "yyyy")
    M497 = R
    Exit Function
EH:
    M497 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M498() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "y")
    M498 = R
    Exit Function
EH:
    M498 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M499() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "q")
    M499 = R
    Exit Function
EH:
    M499 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M500() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "w")
    M500 = R
    Exit Function
EH:
    M500 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M501() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "ww")
    M501 = R
    Exit Function
EH:
    M501 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M502() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "h")
    M502 = R
    Exit Function
EH:
    M502 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M503() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "hh")
    M503 = R
    Exit Function
EH:
    M503 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M504() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "n")
    M504 = R
    Exit Function
EH:
    M504 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M505() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "nn")
    M505 = R
    Exit Function
EH:
    M505 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M506() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "s")
    M506 = R
    Exit Function
EH:
    M506 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M507() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "ss")
    M507 = R
    Exit Function
EH:
    M507 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M508() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "ttttt")
    M508 = R
    Exit Function
EH:
    M508 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M509() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "AM/PM")
    M509 = R
    Exit Function
EH:
    M509 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M510() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "am/pm")
    M510 = R
    Exit Function
EH:
    M510 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M511() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "A/P")
    M511 = R
    Exit Function
EH:
    M511 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M512() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "c")
    M512 = R
    Exit Function
EH:
    M512 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M513() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "yyyy-mm-dd hh:nn:ss")
    M513 = R
    Exit Function
EH:
    M513 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M514() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "h:mm AM/PM")
    M514 = R
    Exit Function
EH:
    M514 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M515() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "mmm d, yyyy")
    M515 = R
    Exit Function
EH:
    M515 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M516() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "[h]:mm")
    M516 = R
    Exit Function
EH:
    M516 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M517() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "oo")
    M517 = R
    Exit Function
EH:
    M517 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M518() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "dd/mmm/yy")
    M518 = R
    Exit Function
EH:
    M518 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M519() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "\d\a\y d")
    M519 = R
    Exit Function
EH:
    M519 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M520() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "d mmmm yyyy h:nn")
    M520 = R
    Exit Function
EH:
    M520 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M521() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "General Date")
    M521 = R
    Exit Function
EH:
    M521 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M522() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "Long Date")
    M522 = R
    Exit Function
EH:
    M522 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M523() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "Medium Date")
    M523 = R
    Exit Function
EH:
    M523 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M524() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "Short Date")
    M524 = R
    Exit Function
EH:
    M524 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M525() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "Long Time")
    M525 = R
    Exit Function
EH:
    M525 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M526() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "Medium Time")
    M526 = R
    Exit Function
EH:
    M526 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M527() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "Short Time")
    M527 = R
    Exit Function
EH:
    M527 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M528() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "mm/dd/yyyy")
    M528 = R
    Exit Function
EH:
    M528 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M529() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "hh:mm:ss.000")
    M529 = R
    Exit Function
EH:
    M529 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M530() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "yyyyq")
    M530 = R
    Exit Function
EH:
    M530 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M531() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#3/5/2001 1:02:03 PM#, "w ww y")
    M531 = R
    Exit Function
EH:
    M531 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M532() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "d")
    M532 = R
    Exit Function
EH:
    M532 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M533() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "dd")
    M533 = R
    Exit Function
EH:
    M533 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M534() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "ddd")
    M534 = R
    Exit Function
EH:
    M534 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M535() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "dddd")
    M535 = R
    Exit Function
EH:
    M535 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M536() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "ddddd")
    M536 = R
    Exit Function
EH:
    M536 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M537() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "dddddd")
    M537 = R
    Exit Function
EH:
    M537 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M538() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "m")
    M538 = R
    Exit Function
EH:
    M538 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M539() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "mm")
    M539 = R
    Exit Function
EH:
    M539 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M540() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "mmm")
    M540 = R
    Exit Function
EH:
    M540 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M541() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "mmmm")
    M541 = R
    Exit Function
EH:
    M541 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M542() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "yy")
    M542 = R
    Exit Function
EH:
    M542 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M543() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "yyyy")
    M543 = R
    Exit Function
EH:
    M543 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M544() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "y")
    M544 = R
    Exit Function
EH:
    M544 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M545() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "q")
    M545 = R
    Exit Function
EH:
    M545 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M546() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "w")
    M546 = R
    Exit Function
EH:
    M546 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M547() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "ww")
    M547 = R
    Exit Function
EH:
    M547 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M548() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "h")
    M548 = R
    Exit Function
EH:
    M548 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M549() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "hh")
    M549 = R
    Exit Function
EH:
    M549 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M550() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "n")
    M550 = R
    Exit Function
EH:
    M550 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M551() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "nn")
    M551 = R
    Exit Function
EH:
    M551 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M552() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "s")
    M552 = R
    Exit Function
EH:
    M552 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M553() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "ss")
    M553 = R
    Exit Function
EH:
    M553 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M554() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "ttttt")
    M554 = R
    Exit Function
EH:
    M554 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M555() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "AM/PM")
    M555 = R
    Exit Function
EH:
    M555 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M556() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "am/pm")
    M556 = R
    Exit Function
EH:
    M556 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M557() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "A/P")
    M557 = R
    Exit Function
EH:
    M557 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M558() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "c")
    M558 = R
    Exit Function
EH:
    M558 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M559() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "yyyy-mm-dd hh:nn:ss")
    M559 = R
    Exit Function
EH:
    M559 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M560() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "h:mm AM/PM")
    M560 = R
    Exit Function
EH:
    M560 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M561() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "mmm d, yyyy")
    M561 = R
    Exit Function
EH:
    M561 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M562() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "[h]:mm")
    M562 = R
    Exit Function
EH:
    M562 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M563() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "oo")
    M563 = R
    Exit Function
EH:
    M563 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M564() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "dd/mmm/yy")
    M564 = R
    Exit Function
EH:
    M564 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M565() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "\d\a\y d")
    M565 = R
    Exit Function
EH:
    M565 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M566() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "d mmmm yyyy h:nn")
    M566 = R
    Exit Function
EH:
    M566 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M567() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "General Date")
    M567 = R
    Exit Function
EH:
    M567 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M568() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "Long Date")
    M568 = R
    Exit Function
EH:
    M568 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M569() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "Medium Date")
    M569 = R
    Exit Function
EH:
    M569 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M570() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "Short Date")
    M570 = R
    Exit Function
EH:
    M570 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M571() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "Long Time")
    M571 = R
    Exit Function
EH:
    M571 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M572() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "Medium Time")
    M572 = R
    Exit Function
EH:
    M572 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M573() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "Short Time")
    M573 = R
    Exit Function
EH:
    M573 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M574() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "mm/dd/yyyy")
    M574 = R
    Exit Function
EH:
    M574 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M575() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "hh:mm:ss.000")
    M575 = R
    Exit Function
EH:
    M575 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M576() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "yyyyq")
    M576 = R
    Exit Function
EH:
    M576 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M577() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#12/31/2000#, "w ww y")
    M577 = R
    Exit Function
EH:
    M577 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M578() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "d")
    M578 = R
    Exit Function
EH:
    M578 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M579() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "dd")
    M579 = R
    Exit Function
EH:
    M579 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M580() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "ddd")
    M580 = R
    Exit Function
EH:
    M580 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M581() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "dddd")
    M581 = R
    Exit Function
EH:
    M581 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M582() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "ddddd")
    M582 = R
    Exit Function
EH:
    M582 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M583() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "dddddd")
    M583 = R
    Exit Function
EH:
    M583 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M584() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "m")
    M584 = R
    Exit Function
EH:
    M584 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M585() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "mm")
    M585 = R
    Exit Function
EH:
    M585 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M586() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "mmm")
    M586 = R
    Exit Function
EH:
    M586 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M587() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "mmmm")
    M587 = R
    Exit Function
EH:
    M587 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M588() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "yy")
    M588 = R
    Exit Function
EH:
    M588 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M589() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "yyyy")
    M589 = R
    Exit Function
EH:
    M589 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M590() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "y")
    M590 = R
    Exit Function
EH:
    M590 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M591() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "q")
    M591 = R
    Exit Function
EH:
    M591 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M592() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "w")
    M592 = R
    Exit Function
EH:
    M592 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M593() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "ww")
    M593 = R
    Exit Function
EH:
    M593 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M594() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "h")
    M594 = R
    Exit Function
EH:
    M594 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M595() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "hh")
    M595 = R
    Exit Function
EH:
    M595 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M596() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "n")
    M596 = R
    Exit Function
EH:
    M596 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M597() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "nn")
    M597 = R
    Exit Function
EH:
    M597 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M598() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "s")
    M598 = R
    Exit Function
EH:
    M598 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M599() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "ss")
    M599 = R
    Exit Function
EH:
    M599 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M600() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "ttttt")
    M600 = R
    Exit Function
EH:
    M600 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M601() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "AM/PM")
    M601 = R
    Exit Function
EH:
    M601 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M602() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "am/pm")
    M602 = R
    Exit Function
EH:
    M602 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M603() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "A/P")
    M603 = R
    Exit Function
EH:
    M603 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M604() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "c")
    M604 = R
    Exit Function
EH:
    M604 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M605() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "yyyy-mm-dd hh:nn:ss")
    M605 = R
    Exit Function
EH:
    M605 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M606() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "h:mm AM/PM")
    M606 = R
    Exit Function
EH:
    M606 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M607() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "mmm d, yyyy")
    M607 = R
    Exit Function
EH:
    M607 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M608() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "[h]:mm")
    M608 = R
    Exit Function
EH:
    M608 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M609() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "oo")
    M609 = R
    Exit Function
EH:
    M609 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M610() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "dd/mmm/yy")
    M610 = R
    Exit Function
EH:
    M610 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M611() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "\d\a\y d")
    M611 = R
    Exit Function
EH:
    M611 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M612() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "d mmmm yyyy h:nn")
    M612 = R
    Exit Function
EH:
    M612 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M613() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "General Date")
    M613 = R
    Exit Function
EH:
    M613 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M614() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "Long Date")
    M614 = R
    Exit Function
EH:
    M614 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M615() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "Medium Date")
    M615 = R
    Exit Function
EH:
    M615 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M616() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "Short Date")
    M616 = R
    Exit Function
EH:
    M616 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M617() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "Long Time")
    M617 = R
    Exit Function
EH:
    M617 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M618() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "Medium Time")
    M618 = R
    Exit Function
EH:
    M618 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M619() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "Short Time")
    M619 = R
    Exit Function
EH:
    M619 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M620() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "mm/dd/yyyy")
    M620 = R
    Exit Function
EH:
    M620 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M621() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "hh:mm:ss.000")
    M621 = R
    Exit Function
EH:
    M621 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M622() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "yyyyq")
    M622 = R
    Exit Function
EH:
    M622 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M623() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005 0:00:00#, "w ww y")
    M623 = R
    Exit Function
EH:
    M623 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M624() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbUseSystemDayOfWeek, vbUseSystem) & "|" & Format(#12/31/2004#, "d", vbUseSystemDayOfWeek, vbUseSystem) & "|" & Format(#1/3/2000#, "d", vbUseSystemDayOfWeek, vbUseSystem)
    M624 = R
    Exit Function
EH:
    M624 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M625() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbUseSystemDayOfWeek, vbFirstJan1) & "|" & Format(#12/31/2004#, "d", vbUseSystemDayOfWeek, vbFirstJan1) & "|" & Format(#1/3/2000#, "d", vbUseSystemDayOfWeek, vbFirstJan1)
    M625 = R
    Exit Function
EH:
    M625 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M626() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbUseSystemDayOfWeek, vbFirstFourDays) & "|" & Format(#12/31/2004#, "d", vbUseSystemDayOfWeek, vbFirstFourDays) & "|" & Format(#1/3/2000#, "d", vbUseSystemDayOfWeek, vbFirstFourDays)
    M626 = R
    Exit Function
EH:
    M626 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M627() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbUseSystemDayOfWeek, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "d", vbUseSystemDayOfWeek, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "d", vbUseSystemDayOfWeek, vbFirstFullWeek)
    M627 = R
    Exit Function
EH:
    M627 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M628() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbSunday, vbUseSystem) & "|" & Format(#12/31/2004#, "d", vbSunday, vbUseSystem) & "|" & Format(#1/3/2000#, "d", vbSunday, vbUseSystem)
    M628 = R
    Exit Function
EH:
    M628 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M629() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbSunday, vbFirstJan1) & "|" & Format(#12/31/2004#, "d", vbSunday, vbFirstJan1) & "|" & Format(#1/3/2000#, "d", vbSunday, vbFirstJan1)
    M629 = R
    Exit Function
EH:
    M629 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M630() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbSunday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "d", vbSunday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "d", vbSunday, vbFirstFourDays)
    M630 = R
    Exit Function
EH:
    M630 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M631() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbSunday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "d", vbSunday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "d", vbSunday, vbFirstFullWeek)
    M631 = R
    Exit Function
EH:
    M631 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M632() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbMonday, vbUseSystem) & "|" & Format(#12/31/2004#, "d", vbMonday, vbUseSystem) & "|" & Format(#1/3/2000#, "d", vbMonday, vbUseSystem)
    M632 = R
    Exit Function
EH:
    M632 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M633() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbMonday, vbFirstJan1) & "|" & Format(#12/31/2004#, "d", vbMonday, vbFirstJan1) & "|" & Format(#1/3/2000#, "d", vbMonday, vbFirstJan1)
    M633 = R
    Exit Function
EH:
    M633 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M634() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbMonday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "d", vbMonday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "d", vbMonday, vbFirstFourDays)
    M634 = R
    Exit Function
EH:
    M634 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M635() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbMonday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "d", vbMonday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "d", vbMonday, vbFirstFullWeek)
    M635 = R
    Exit Function
EH:
    M635 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M636() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbTuesday, vbUseSystem) & "|" & Format(#12/31/2004#, "d", vbTuesday, vbUseSystem) & "|" & Format(#1/3/2000#, "d", vbTuesday, vbUseSystem)
    M636 = R
    Exit Function
EH:
    M636 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M637() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbTuesday, vbFirstJan1) & "|" & Format(#12/31/2004#, "d", vbTuesday, vbFirstJan1) & "|" & Format(#1/3/2000#, "d", vbTuesday, vbFirstJan1)
    M637 = R
    Exit Function
EH:
    M637 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M638() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbTuesday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "d", vbTuesday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "d", vbTuesday, vbFirstFourDays)
    M638 = R
    Exit Function
EH:
    M638 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M639() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbTuesday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "d", vbTuesday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "d", vbTuesday, vbFirstFullWeek)
    M639 = R
    Exit Function
EH:
    M639 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M640() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbSaturday, vbUseSystem) & "|" & Format(#12/31/2004#, "d", vbSaturday, vbUseSystem) & "|" & Format(#1/3/2000#, "d", vbSaturday, vbUseSystem)
    M640 = R
    Exit Function
EH:
    M640 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M641() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbSaturday, vbFirstJan1) & "|" & Format(#12/31/2004#, "d", vbSaturday, vbFirstJan1) & "|" & Format(#1/3/2000#, "d", vbSaturday, vbFirstJan1)
    M641 = R
    Exit Function
EH:
    M641 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M642() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbSaturday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "d", vbSaturday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "d", vbSaturday, vbFirstFourDays)
    M642 = R
    Exit Function
EH:
    M642 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M643() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "d", vbSaturday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "d", vbSaturday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "d", vbSaturday, vbFirstFullWeek)
    M643 = R
    Exit Function
EH:
    M643 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M644() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbUseSystemDayOfWeek, vbUseSystem) & "|" & Format(#12/31/2004#, "ww", vbUseSystemDayOfWeek, vbUseSystem) & "|" & Format(#1/3/2000#, "ww", vbUseSystemDayOfWeek, vbUseSystem)
    M644 = R
    Exit Function
EH:
    M644 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M645() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbUseSystemDayOfWeek, vbFirstJan1) & "|" & Format(#12/31/2004#, "ww", vbUseSystemDayOfWeek, vbFirstJan1) & "|" & Format(#1/3/2000#, "ww", vbUseSystemDayOfWeek, vbFirstJan1)
    M645 = R
    Exit Function
EH:
    M645 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M646() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbUseSystemDayOfWeek, vbFirstFourDays) & "|" & Format(#12/31/2004#, "ww", vbUseSystemDayOfWeek, vbFirstFourDays) & "|" & Format(#1/3/2000#, "ww", vbUseSystemDayOfWeek, vbFirstFourDays)
    M646 = R
    Exit Function
EH:
    M646 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M647() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbUseSystemDayOfWeek, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "ww", vbUseSystemDayOfWeek, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "ww", vbUseSystemDayOfWeek, vbFirstFullWeek)
    M647 = R
    Exit Function
EH:
    M647 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M648() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbSunday, vbUseSystem) & "|" & Format(#12/31/2004#, "ww", vbSunday, vbUseSystem) & "|" & Format(#1/3/2000#, "ww", vbSunday, vbUseSystem)
    M648 = R
    Exit Function
EH:
    M648 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M649() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbSunday, vbFirstJan1) & "|" & Format(#12/31/2004#, "ww", vbSunday, vbFirstJan1) & "|" & Format(#1/3/2000#, "ww", vbSunday, vbFirstJan1)
    M649 = R
    Exit Function
EH:
    M649 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M650() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbSunday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "ww", vbSunday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "ww", vbSunday, vbFirstFourDays)
    M650 = R
    Exit Function
EH:
    M650 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M651() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbSunday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "ww", vbSunday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "ww", vbSunday, vbFirstFullWeek)
    M651 = R
    Exit Function
EH:
    M651 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M652() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbMonday, vbUseSystem) & "|" & Format(#12/31/2004#, "ww", vbMonday, vbUseSystem) & "|" & Format(#1/3/2000#, "ww", vbMonday, vbUseSystem)
    M652 = R
    Exit Function
EH:
    M652 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M653() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbMonday, vbFirstJan1) & "|" & Format(#12/31/2004#, "ww", vbMonday, vbFirstJan1) & "|" & Format(#1/3/2000#, "ww", vbMonday, vbFirstJan1)
    M653 = R
    Exit Function
EH:
    M653 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M654() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbMonday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "ww", vbMonday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "ww", vbMonday, vbFirstFourDays)
    M654 = R
    Exit Function
EH:
    M654 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M655() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbMonday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "ww", vbMonday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "ww", vbMonday, vbFirstFullWeek)
    M655 = R
    Exit Function
EH:
    M655 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M656() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbTuesday, vbUseSystem) & "|" & Format(#12/31/2004#, "ww", vbTuesday, vbUseSystem) & "|" & Format(#1/3/2000#, "ww", vbTuesday, vbUseSystem)
    M656 = R
    Exit Function
EH:
    M656 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M657() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbTuesday, vbFirstJan1) & "|" & Format(#12/31/2004#, "ww", vbTuesday, vbFirstJan1) & "|" & Format(#1/3/2000#, "ww", vbTuesday, vbFirstJan1)
    M657 = R
    Exit Function
EH:
    M657 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M658() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbTuesday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "ww", vbTuesday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "ww", vbTuesday, vbFirstFourDays)
    M658 = R
    Exit Function
EH:
    M658 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M659() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbTuesday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "ww", vbTuesday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "ww", vbTuesday, vbFirstFullWeek)
    M659 = R
    Exit Function
EH:
    M659 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M660() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbSaturday, vbUseSystem) & "|" & Format(#12/31/2004#, "ww", vbSaturday, vbUseSystem) & "|" & Format(#1/3/2000#, "ww", vbSaturday, vbUseSystem)
    M660 = R
    Exit Function
EH:
    M660 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M661() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbSaturday, vbFirstJan1) & "|" & Format(#12/31/2004#, "ww", vbSaturday, vbFirstJan1) & "|" & Format(#1/3/2000#, "ww", vbSaturday, vbFirstJan1)
    M661 = R
    Exit Function
EH:
    M661 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M662() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbSaturday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "ww", vbSaturday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "ww", vbSaturday, vbFirstFourDays)
    M662 = R
    Exit Function
EH:
    M662 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M663() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "ww", vbSaturday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "ww", vbSaturday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "ww", vbSaturday, vbFirstFullWeek)
    M663 = R
    Exit Function
EH:
    M663 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M664() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbUseSystemDayOfWeek, vbUseSystem) & "|" & Format(#12/31/2004#, "w", vbUseSystemDayOfWeek, vbUseSystem) & "|" & Format(#1/3/2000#, "w", vbUseSystemDayOfWeek, vbUseSystem)
    M664 = R
    Exit Function
EH:
    M664 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M665() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbUseSystemDayOfWeek, vbFirstJan1) & "|" & Format(#12/31/2004#, "w", vbUseSystemDayOfWeek, vbFirstJan1) & "|" & Format(#1/3/2000#, "w", vbUseSystemDayOfWeek, vbFirstJan1)
    M665 = R
    Exit Function
EH:
    M665 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M666() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbUseSystemDayOfWeek, vbFirstFourDays) & "|" & Format(#12/31/2004#, "w", vbUseSystemDayOfWeek, vbFirstFourDays) & "|" & Format(#1/3/2000#, "w", vbUseSystemDayOfWeek, vbFirstFourDays)
    M666 = R
    Exit Function
EH:
    M666 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M667() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbUseSystemDayOfWeek, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "w", vbUseSystemDayOfWeek, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "w", vbUseSystemDayOfWeek, vbFirstFullWeek)
    M667 = R
    Exit Function
EH:
    M667 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M668() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbSunday, vbUseSystem) & "|" & Format(#12/31/2004#, "w", vbSunday, vbUseSystem) & "|" & Format(#1/3/2000#, "w", vbSunday, vbUseSystem)
    M668 = R
    Exit Function
EH:
    M668 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M669() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbSunday, vbFirstJan1) & "|" & Format(#12/31/2004#, "w", vbSunday, vbFirstJan1) & "|" & Format(#1/3/2000#, "w", vbSunday, vbFirstJan1)
    M669 = R
    Exit Function
EH:
    M669 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M670() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbSunday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "w", vbSunday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "w", vbSunday, vbFirstFourDays)
    M670 = R
    Exit Function
EH:
    M670 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M671() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbSunday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "w", vbSunday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "w", vbSunday, vbFirstFullWeek)
    M671 = R
    Exit Function
EH:
    M671 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M672() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbMonday, vbUseSystem) & "|" & Format(#12/31/2004#, "w", vbMonday, vbUseSystem) & "|" & Format(#1/3/2000#, "w", vbMonday, vbUseSystem)
    M672 = R
    Exit Function
EH:
    M672 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M673() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbMonday, vbFirstJan1) & "|" & Format(#12/31/2004#, "w", vbMonday, vbFirstJan1) & "|" & Format(#1/3/2000#, "w", vbMonday, vbFirstJan1)
    M673 = R
    Exit Function
EH:
    M673 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M674() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbMonday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "w", vbMonday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "w", vbMonday, vbFirstFourDays)
    M674 = R
    Exit Function
EH:
    M674 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M675() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbMonday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "w", vbMonday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "w", vbMonday, vbFirstFullWeek)
    M675 = R
    Exit Function
EH:
    M675 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M676() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbTuesday, vbUseSystem) & "|" & Format(#12/31/2004#, "w", vbTuesday, vbUseSystem) & "|" & Format(#1/3/2000#, "w", vbTuesday, vbUseSystem)
    M676 = R
    Exit Function
EH:
    M676 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M677() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbTuesday, vbFirstJan1) & "|" & Format(#12/31/2004#, "w", vbTuesday, vbFirstJan1) & "|" & Format(#1/3/2000#, "w", vbTuesday, vbFirstJan1)
    M677 = R
    Exit Function
EH:
    M677 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M678() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbTuesday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "w", vbTuesday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "w", vbTuesday, vbFirstFourDays)
    M678 = R
    Exit Function
EH:
    M678 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M679() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbTuesday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "w", vbTuesday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "w", vbTuesday, vbFirstFullWeek)
    M679 = R
    Exit Function
EH:
    M679 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M680() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbSaturday, vbUseSystem) & "|" & Format(#12/31/2004#, "w", vbSaturday, vbUseSystem) & "|" & Format(#1/3/2000#, "w", vbSaturday, vbUseSystem)
    M680 = R
    Exit Function
EH:
    M680 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M681() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbSaturday, vbFirstJan1) & "|" & Format(#12/31/2004#, "w", vbSaturday, vbFirstJan1) & "|" & Format(#1/3/2000#, "w", vbSaturday, vbFirstJan1)
    M681 = R
    Exit Function
EH:
    M681 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M682() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbSaturday, vbFirstFourDays) & "|" & Format(#12/31/2004#, "w", vbSaturday, vbFirstFourDays) & "|" & Format(#1/3/2000#, "w", vbSaturday, vbFirstFourDays)
    M682 = R
    Exit Function
EH:
    M682 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M683() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(#1/1/2005#, "w", vbSaturday, vbFirstFullWeek) & "|" & Format(#12/31/2004#, "w", vbSaturday, vbFirstFullWeek) & "|" & Format(#1/3/2000#, "w", vbSaturday, vbFirstFullWeek)
    M683 = R
    Exit Function
EH:
    M683 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M684() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(Null, "0")
    M684 = R
    Exit Function
EH:
    M684 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M685() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(Empty, "0")
    M685 = R
    Exit Function
EH:
    M685 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M686() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(Empty)
    M686 = R
    Exit Function
EH:
    M686 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M687() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(Null)
    M687 = R
    Exit Function
EH:
    M687 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M688() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("abc", ">")
    M688 = R
    Exit Function
EH:
    M688 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M689() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("abc", "<")
    M689 = R
    Exit Function
EH:
    M689 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M690() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("abc", "@@@@@@")
    M690 = R
    Exit Function
EH:
    M690 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M691() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("abc", "!@@@@@@")
    M691 = R
    Exit Function
EH:
    M691 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M692() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("abc", "&&&&&&")
    M692 = R
    Exit Function
EH:
    M692 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M693() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("", "@;empty")
    M693 = R
    Exit Function
EH:
    M693 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M694() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("", "x;y;z;w")
    M694 = R
    Exit Function
EH:
    M694 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M695() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(5, "x;y;z;w")
    M695 = R
    Exit Function
EH:
    M695 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M696() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "x;y;z;w")
    M696 = R
    Exit Function
EH:
    M696 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M697() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-5, "x;y;z;w")
    M697 = R
    Exit Function
EH:
    M697 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M698() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(Null, "x;y;z;w")
    M698 = R
    Exit Function
EH:
    M698 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M699() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("5", "0.00")
    M699 = R
    Exit Function
EH:
    M699 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M700() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("abc", "0.00")
    M700 = R
    Exit Function
EH:
    M700 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M701() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("1/2/2003", "yyyy")
    M701 = R
    Exit Function
EH:
    M701 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M702() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("12:30", "h")
    M702 = R
    Exit Function
EH:
    M702 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M703() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(True, "Yes/No")
    M703 = R
    Exit Function
EH:
    M703 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M704() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(True)
    M704 = R
    Exit Function
EH:
    M704 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M705() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(True, "0")
    M705 = R
    Exit Function
EH:
    M705 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M706() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(CVErr(7))
    M706 = R
    Exit Function
EH:
    M706 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M707() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "")
    M707 = R
    Exit Function
EH:
    M707 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M708() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, Null)
    M708 = R
    Exit Function
EH:
    M708 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M709() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, 5)
    M709 = R
    Exit Function
EH:
    M709 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M710() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1.5, "0", 1)
    M710 = R
    Exit Function
EH:
    M710 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M711() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1.5, "0", 8)
    M711 = R
    Exit Function
EH:
    M711 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M712() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0", , 9)
    M712 = R
    Exit Function
EH:
    M712 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M713() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1, "0", 0, 0)
    M713 = R
    Exit Function
EH:
    M713 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M714() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(Array(1, 2))
    M714 = R
    Exit Function
EH:
    M714 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M715() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(New Collection)
    M715 = R
    Exit Function
EH:
    M715 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M716() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E+30, "0")
    M716 = R
    Exit Function
EH:
    M716 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M717() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1E+30)
    M717 = R
    Exit Function
EH:
    M717 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M718() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(-1E-30, "0.0000")
    M718 = R
    Exit Function
EH:
    M718 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M719() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.1 + 0.2, "0.000000000000000000")
    M719 = R
    Exit Function
EH:
    M719 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M720() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(CDec("1.23456789012345678"), "0.00000000000000000000")
    M720 = R
    Exit Function
EH:
    M720 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M721() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(CCur(1234.5678), "#,##0.0000")
    M721 = R
    Exit Function
EH:
    M721 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M722() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(123456789012345#, "0")
    M722 = R
    Exit Function
EH:
    M722 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M723() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.5, "\$#,##0.00")
    M723 = R
    Exit Function
EH:
    M723 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M724() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.5, "0%") & Format(1.5, "0%") & Format(2.5, "0%") & Format(-0.5, "0%")
    M724 = R
    Exit Function
EH:
    M724 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M725() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1.5, "0") & Format(2.5, "0") & Format(0.5, "0") & Format(-2.5, "0")
    M725 = R
    Exit Function
EH:
    M725 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M726() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(12345.6789, "#,##0.00;;;") & "|" & Format(0, "#,##0.00;;;")
    M726 = R
    Exit Function
EH:
    M726 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M727() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(5, "[Red]0") & Format(5, "[Blue]0.0") & Format(-5, "0;[Red]-0")
    M727 = R
    Exit Function
EH:
    M727 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M728() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234, "#,#") & "|" & Format(1234567, "#,,") & "|" & Format(1234567, "0.0,,")
    M728 = R
    Exit Function
EH:
    M728 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M729() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0.000123, "0.00E-00") & "|" & Format(123456, "0.0E+0") & "|" & Format(0, "0.0E+0")
    M729 = R
    Exit Function
EH:
    M729 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M730() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(5, "00000") & "|" & Format(5.5, "00.00") & "|" & Format(-5.5, "00.00") & "|" & Format(0.5, "#.00") & "|" & Format(0.5, "0.#")
    M730 = R
    Exit Function
EH:
    M730 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M731() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format("hello", "\<@\>") & "|" & Format("hello", "@@@@@@@@@@") & "|" & Format("hello", "!@@@@@@@@@@") & "|" & Format("Hello", "<@") & "|" & Format("Hello", ">@")
    M731 = R
    Exit Function
EH:
    M731 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M732() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(1234.567, "##0.0") & "|" & Format(-1234.567, "(#,##0.00)") & "|" & Format(1234.567, "#,##0.00 ""USD""")
    M732 = R
    Exit Function
EH:
    M732 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M733() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Format(0, "#") & "|" & Format(0, "#.#") & "|" & Format(0, "0.#") & "|" & Format(0.04, "0.0") & "|" & Format(0.05, "0.0") & "|" & Format(0.15, "0.0") & "|" & Format(0.25, "0.0")
    M733 = R
    Exit Function
EH:
    M733 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M734() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("")) & "|" & Dump(fV(IsNumeric(""))) & "|" & Dump(fV(IsDate("")))
    M734 = R
    Exit Function
EH:
    M734 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M735() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val(" ")) & "|" & Dump(fV(IsNumeric(" "))) & "|" & Dump(fV(IsDate(" ")))
    M735 = R
    Exit Function
EH:
    M735 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M736() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1")) & "|" & Dump(fV(IsNumeric("1"))) & "|" & Dump(fV(IsDate("1")))
    M736 = R
    Exit Function
EH:
    M736 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M737() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val(" 12 ")) & "|" & Dump(fV(IsNumeric(" 12 "))) & "|" & Dump(fV(IsDate(" 12 ")))
    M737 = R
    Exit Function
EH:
    M737 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M738() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1 2 3")) & "|" & Dump(fV(IsNumeric("1 2 3"))) & "|" & Dump(fV(IsDate("1 2 3")))
    M738 = R
    Exit Function
EH:
    M738 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M739() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&H10")) & "|" & Dump(fV(IsNumeric("&H10"))) & "|" & Dump(fV(IsDate("&H10")))
    M739 = R
    Exit Function
EH:
    M739 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M740() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&O17")) & "|" & Dump(fV(IsNumeric("&O17"))) & "|" & Dump(fV(IsDate("&O17")))
    M740 = R
    Exit Function
EH:
    M740 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M741() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&HFFFF")) & "|" & Dump(fV(IsNumeric("&HFFFF"))) & "|" & Dump(fV(IsDate("&HFFFF")))
    M741 = R
    Exit Function
EH:
    M741 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M742() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&HFFFFFFFF")) & "|" & Dump(fV(IsNumeric("&HFFFFFFFF"))) & "|" & Dump(fV(IsDate("&HFFFFFFFF")))
    M742 = R
    Exit Function
EH:
    M742 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M743() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&H")) & "|" & Dump(fV(IsNumeric("&H"))) & "|" & Dump(fV(IsDate("&H")))
    M743 = R
    Exit Function
EH:
    M743 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M744() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1e5")) & "|" & Dump(fV(IsNumeric("1e5"))) & "|" & Dump(fV(IsDate("1e5")))
    M744 = R
    Exit Function
EH:
    M744 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M745() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1d5")) & "|" & Dump(fV(IsNumeric("1d5"))) & "|" & Dump(fV(IsDate("1d5")))
    M745 = R
    Exit Function
EH:
    M745 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M746() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1D+2")) & "|" & Dump(fV(IsNumeric("1D+2"))) & "|" & Dump(fV(IsDate("1D+2")))
    M746 = R
    Exit Function
EH:
    M746 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M747() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1E")) & "|" & Dump(fV(IsNumeric("1E"))) & "|" & Dump(fV(IsDate("1E")))
    M747 = R
    Exit Function
EH:
    M747 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M748() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1E+")) & "|" & Dump(fV(IsNumeric("1E+"))) & "|" & Dump(fV(IsDate("1E+")))
    M748 = R
    Exit Function
EH:
    M748 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M749() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("+5")) & "|" & Dump(fV(IsNumeric("+5"))) & "|" & Dump(fV(IsDate("+5")))
    M749 = R
    Exit Function
EH:
    M749 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M750() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("-5")) & "|" & Dump(fV(IsNumeric("-5"))) & "|" & Dump(fV(IsDate("-5")))
    M750 = R
    Exit Function
EH:
    M750 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M751() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("--5")) & "|" & Dump(fV(IsNumeric("--5"))) & "|" & Dump(fV(IsDate("--5")))
    M751 = R
    Exit Function
EH:
    M751 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M752() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("+-5")) & "|" & Dump(fV(IsNumeric("+-5"))) & "|" & Dump(fV(IsDate("+-5")))
    M752 = R
    Exit Function
EH:
    M752 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M753() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val(".5.")) & "|" & Dump(fV(IsNumeric(".5."))) & "|" & Dump(fV(IsDate(".5.")))
    M753 = R
    Exit Function
EH:
    M753 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M754() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("5.5.5")) & "|" & Dump(fV(IsNumeric("5.5.5"))) & "|" & Dump(fV(IsDate("5.5.5")))
    M754 = R
    Exit Function
EH:
    M754 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M755() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1,000")) & "|" & Dump(fV(IsNumeric("1,000"))) & "|" & Dump(fV(IsDate("1,000")))
    M755 = R
    Exit Function
EH:
    M755 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M756() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1" & vbTab & "2")) & "|" & Dump(fV(IsNumeric("1" & vbTab & "2"))) & "|" & Dump(fV(IsDate("1" & vbTab & "2")))
    M756 = R
    Exit Function
EH:
    M756 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M757() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("12" & vbCrLf & "3")) & "|" & Dump(fV(IsNumeric("12" & vbCrLf & "3"))) & "|" & Dump(fV(IsDate("12" & vbCrLf & "3")))
    M757 = R
    Exit Function
EH:
    M757 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M758() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("0x10")) & "|" & Dump(fV(IsNumeric("0x10"))) & "|" & Dump(fV(IsDate("0x10")))
    M758 = R
    Exit Function
EH:
    M758 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M759() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&H1G")) & "|" & Dump(fV(IsNumeric("&H1G"))) & "|" & Dump(fV(IsDate("&H1G")))
    M759 = R
    Exit Function
EH:
    M759 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M760() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&&H10")) & "|" & Dump(fV(IsNumeric("&&H10"))) & "|" & Dump(fV(IsDate("&&H10")))
    M760 = R
    Exit Function
EH:
    M760 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M761() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("- 5")) & "|" & Dump(fV(IsNumeric("- 5"))) & "|" & Dump(fV(IsDate("- 5")))
    M761 = R
    Exit Function
EH:
    M761 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M762() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("5 -")) & "|" & Dump(fV(IsNumeric("5 -"))) & "|" & Dump(fV(IsDate("5 -")))
    M762 = R
    Exit Function
EH:
    M762 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M763() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("$5")) & "|" & Dump(fV(IsNumeric("$5"))) & "|" & Dump(fV(IsDate("$5")))
    M763 = R
    Exit Function
EH:
    M763 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M764() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("5%")) & "|" & Dump(fV(IsNumeric("5%"))) & "|" & Dump(fV(IsDate("5%")))
    M764 = R
    Exit Function
EH:
    M764 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M765() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("abc")) & "|" & Dump(fV(IsNumeric("abc"))) & "|" & Dump(fV(IsDate("abc")))
    M765 = R
    Exit Function
EH:
    M765 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M766() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1.5e+3x")) & "|" & Dump(fV(IsNumeric("1.5e+3x"))) & "|" & Dump(fV(IsDate("1.5e+3x")))
    M766 = R
    Exit Function
EH:
    M766 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M767() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("-.e5")) & "|" & Dump(fV(IsNumeric("-.e5"))) & "|" & Dump(fV(IsDate("-.e5")))
    M767 = R
    Exit Function
EH:
    M767 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M768() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val(".")) & "|" & Dump(fV(IsNumeric("."))) & "|" & Dump(fV(IsDate(".")))
    M768 = R
    Exit Function
EH:
    M768 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M769() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("-")) & "|" & Dump(fV(IsNumeric("-"))) & "|" & Dump(fV(IsDate("-")))
    M769 = R
    Exit Function
EH:
    M769 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M770() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("0.1234567890123456789")) & "|" & Dump(fV(IsNumeric("0.1234567890123456789"))) & "|" & Dump(fV(IsDate("0.1234567890123456789")))
    M770 = R
    Exit Function
EH:
    M770 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M771() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1" & ChrW(160) & "2")) & "|" & Dump(fV(IsNumeric("1" & ChrW(160) & "2"))) & "|" & Dump(fV(IsDate("1" & ChrW(160) & "2")))
    M771 = R
    Exit Function
EH:
    M771 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M772() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("  &H10")) & "|" & Dump(fV(IsNumeric("  &H10"))) & "|" & Dump(fV(IsDate("  &H10")))
    M772 = R
    Exit Function
EH:
    M772 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M773() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&h10")) & "|" & Dump(fV(IsNumeric("&h10"))) & "|" & Dump(fV(IsDate("&h10")))
    M773 = R
    Exit Function
EH:
    M773 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M774() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&o10")) & "|" & Dump(fV(IsNumeric("&o10"))) & "|" & Dump(fV(IsDate("&o10")))
    M774 = R
    Exit Function
EH:
    M774 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M775() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("" & ChrW(1635) & "")) & "|" & Dump(fV(IsNumeric("" & ChrW(1635) & ""))) & "|" & Dump(fV(IsDate("" & ChrW(1635) & "")))
    M775 = R
    Exit Function
EH:
    M775 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M776() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("" & ChrW(65299) & "")) & "|" & Dump(fV(IsNumeric("" & ChrW(65299) & ""))) & "|" & Dump(fV(IsDate("" & ChrW(65299) & "")))
    M776 = R
    Exit Function
EH:
    M776 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M777() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1E308")) & "|" & Dump(fV(IsNumeric("1E308"))) & "|" & Dump(fV(IsDate("1E308")))
    M777 = R
    Exit Function
EH:
    M777 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M778() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1E309")) & "|" & Dump(fV(IsNumeric("1E309"))) & "|" & Dump(fV(IsDate("1E309")))
    M778 = R
    Exit Function
EH:
    M778 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M779() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("-1E-400")) & "|" & Dump(fV(IsNumeric("-1E-400"))) & "|" & Dump(fV(IsDate("-1E-400")))
    M779 = R
    Exit Function
EH:
    M779 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M780() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("00012")) & "|" & Dump(fV(IsNumeric("00012"))) & "|" & Dump(fV(IsDate("00012")))
    M780 = R
    Exit Function
EH:
    M780 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M781() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("12345678901234567890")) & "|" & Dump(fV(IsNumeric("12345678901234567890"))) & "|" & Dump(fV(IsDate("12345678901234567890")))
    M781 = R
    Exit Function
EH:
    M781 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M782() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("1e+05")) & "|" & Dump(fV(IsNumeric("1e+05"))) & "|" & Dump(fV(IsDate("1e+05")))
    M782 = R
    Exit Function
EH:
    M782 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M783() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&H8000")) & "|" & Dump(fV(IsNumeric("&H8000"))) & "|" & Dump(fV(IsDate("&H8000")))
    M783 = R
    Exit Function
EH:
    M783 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M784() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&H7FFFFFFF&")) & "|" & Dump(fV(IsNumeric("&H7FFFFFFF&"))) & "|" & Dump(fV(IsDate("&H7FFFFFFF&")))
    M784 = R
    Exit Function
EH:
    M784 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M785() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(Val("&HFFFF&")) & "|" & Dump(fV(IsNumeric("&HFFFF&"))) & "|" & Dump(fV(IsDate("&HFFFF&")))
    M785 = R
    Exit Function
EH:
    M785 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M786() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl(""))
    M786 = R
    Exit Function
EH:
    M786 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M787() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl(" "))
    M787 = R
    Exit Function
EH:
    M787 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M788() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1"))
    M788 = R
    Exit Function
EH:
    M788 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M789() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl(" 12 "))
    M789 = R
    Exit Function
EH:
    M789 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M790() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1 2 3"))
    M790 = R
    Exit Function
EH:
    M790 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M791() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&H10"))
    M791 = R
    Exit Function
EH:
    M791 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M792() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&O17"))
    M792 = R
    Exit Function
EH:
    M792 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M793() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&HFFFF"))
    M793 = R
    Exit Function
EH:
    M793 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M794() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&HFFFFFFFF"))
    M794 = R
    Exit Function
EH:
    M794 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M795() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&H"))
    M795 = R
    Exit Function
EH:
    M795 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M796() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1e5"))
    M796 = R
    Exit Function
EH:
    M796 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M797() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1d5"))
    M797 = R
    Exit Function
EH:
    M797 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M798() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1D+2"))
    M798 = R
    Exit Function
EH:
    M798 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M799() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1E"))
    M799 = R
    Exit Function
EH:
    M799 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M800() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1E+"))
    M800 = R
    Exit Function
EH:
    M800 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M801() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("+5"))
    M801 = R
    Exit Function
EH:
    M801 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M802() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("-5"))
    M802 = R
    Exit Function
EH:
    M802 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M803() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("--5"))
    M803 = R
    Exit Function
EH:
    M803 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M804() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("+-5"))
    M804 = R
    Exit Function
EH:
    M804 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M805() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl(".5."))
    M805 = R
    Exit Function
EH:
    M805 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M806() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("5.5.5"))
    M806 = R
    Exit Function
EH:
    M806 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M807() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1,000"))
    M807 = R
    Exit Function
EH:
    M807 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M808() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1" & vbTab & "2"))
    M808 = R
    Exit Function
EH:
    M808 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M809() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("12" & vbCrLf & "3"))
    M809 = R
    Exit Function
EH:
    M809 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M810() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("0x10"))
    M810 = R
    Exit Function
EH:
    M810 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M811() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&H1G"))
    M811 = R
    Exit Function
EH:
    M811 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M812() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&&H10"))
    M812 = R
    Exit Function
EH:
    M812 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M813() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("- 5"))
    M813 = R
    Exit Function
EH:
    M813 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M814() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("5 -"))
    M814 = R
    Exit Function
EH:
    M814 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M815() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("$5"))
    M815 = R
    Exit Function
EH:
    M815 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M816() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("5%"))
    M816 = R
    Exit Function
EH:
    M816 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M817() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("abc"))
    M817 = R
    Exit Function
EH:
    M817 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M818() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1.5e+3x"))
    M818 = R
    Exit Function
EH:
    M818 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M819() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("-.e5"))
    M819 = R
    Exit Function
EH:
    M819 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M820() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("."))
    M820 = R
    Exit Function
EH:
    M820 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M821() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("-"))
    M821 = R
    Exit Function
EH:
    M821 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M822() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("0.1234567890123456789"))
    M822 = R
    Exit Function
EH:
    M822 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M823() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1" & ChrW(160) & "2"))
    M823 = R
    Exit Function
EH:
    M823 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M824() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("  &H10"))
    M824 = R
    Exit Function
EH:
    M824 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M825() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&h10"))
    M825 = R
    Exit Function
EH:
    M825 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M826() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&o10"))
    M826 = R
    Exit Function
EH:
    M826 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M827() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("" & ChrW(1635) & ""))
    M827 = R
    Exit Function
EH:
    M827 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M828() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("" & ChrW(65299) & ""))
    M828 = R
    Exit Function
EH:
    M828 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M829() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1E308"))
    M829 = R
    Exit Function
EH:
    M829 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M830() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1E309"))
    M830 = R
    Exit Function
EH:
    M830 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M831() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("-1E-400"))
    M831 = R
    Exit Function
EH:
    M831 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M832() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("00012"))
    M832 = R
    Exit Function
EH:
    M832 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M833() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("12345678901234567890"))
    M833 = R
    Exit Function
EH:
    M833 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M834() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("1e+05"))
    M834 = R
    Exit Function
EH:
    M834 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M835() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&H8000"))
    M835 = R
    Exit Function
EH:
    M835 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M836() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&H7FFFFFFF&"))
    M836 = R
    Exit Function
EH:
    M836 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M837() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(CDbl("&HFFFF&"))
    M837 = R
    Exit Function
EH:
    M837 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M838() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(""))) & Dump(fV(CLng("")))
    M838 = R
    Exit Function
EH:
    M838 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M839() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(""))) & Dump(fV(CCur("")))
    M839 = R
    Exit Function
EH:
    M839 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M840() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(""))) & Dump(fV(CSng("")))
    M840 = R
    Exit Function
EH:
    M840 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M841() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("")))
    M841 = R
    Exit Function
EH:
    M841 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M842() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("")))
    M842 = R
    Exit Function
EH:
    M842 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M843() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("1"))) & Dump(fV(CLng("1")))
    M843 = R
    Exit Function
EH:
    M843 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M844() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("1"))) & Dump(fV(CCur("1")))
    M844 = R
    Exit Function
EH:
    M844 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M845() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("1"))) & Dump(fV(CSng("1")))
    M845 = R
    Exit Function
EH:
    M845 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M846() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1")))
    M846 = R
    Exit Function
EH:
    M846 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M847() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("1")))
    M847 = R
    Exit Function
EH:
    M847 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M848() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("&H10"))) & Dump(fV(CLng("&H10")))
    M848 = R
    Exit Function
EH:
    M848 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M849() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("&H10"))) & Dump(fV(CCur("&H10")))
    M849 = R
    Exit Function
EH:
    M849 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M850() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("&H10"))) & Dump(fV(CSng("&H10")))
    M850 = R
    Exit Function
EH:
    M850 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M851() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("&H10")))
    M851 = R
    Exit Function
EH:
    M851 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M852() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("&H10")))
    M852 = R
    Exit Function
EH:
    M852 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M853() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("1e5"))) & Dump(fV(CLng("1e5")))
    M853 = R
    Exit Function
EH:
    M853 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M854() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("1e5"))) & Dump(fV(CCur("1e5")))
    M854 = R
    Exit Function
EH:
    M854 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M855() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("1e5"))) & Dump(fV(CSng("1e5")))
    M855 = R
    Exit Function
EH:
    M855 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M856() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1e5")))
    M856 = R
    Exit Function
EH:
    M856 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M857() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("1e5")))
    M857 = R
    Exit Function
EH:
    M857 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M858() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("1,5"))) & Dump(fV(CLng("1,5")))
    M858 = R
    Exit Function
EH:
    M858 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M859() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("1,5"))) & Dump(fV(CCur("1,5")))
    M859 = R
    Exit Function
EH:
    M859 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M860() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("1,5"))) & Dump(fV(CSng("1,5")))
    M860 = R
    Exit Function
EH:
    M860 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M861() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1,5")))
    M861 = R
    Exit Function
EH:
    M861 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M862() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("1,5")))
    M862 = R
    Exit Function
EH:
    M862 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M863() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(" 12 "))) & Dump(fV(CLng(" 12 ")))
    M863 = R
    Exit Function
EH:
    M863 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M864() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(" 12 "))) & Dump(fV(CCur(" 12 ")))
    M864 = R
    Exit Function
EH:
    M864 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M865() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(" 12 "))) & Dump(fV(CSng(" 12 ")))
    M865 = R
    Exit Function
EH:
    M865 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M866() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate(" 12 ")))
    M866 = R
    Exit Function
EH:
    M866 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M867() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(" 12 ")))
    M867 = R
    Exit Function
EH:
    M867 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M868() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("1.5"))) & Dump(fV(CLng("1.5")))
    M868 = R
    Exit Function
EH:
    M868 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M869() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("1.5"))) & Dump(fV(CCur("1.5")))
    M869 = R
    Exit Function
EH:
    M869 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M870() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("1.5"))) & Dump(fV(CSng("1.5")))
    M870 = R
    Exit Function
EH:
    M870 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M871() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1.5")))
    M871 = R
    Exit Function
EH:
    M871 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M872() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("1.5")))
    M872 = R
    Exit Function
EH:
    M872 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M873() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("2.5"))) & Dump(fV(CLng("2.5")))
    M873 = R
    Exit Function
EH:
    M873 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M874() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("2.5"))) & Dump(fV(CCur("2.5")))
    M874 = R
    Exit Function
EH:
    M874 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M875() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("2.5"))) & Dump(fV(CSng("2.5")))
    M875 = R
    Exit Function
EH:
    M875 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M876() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("2.5")))
    M876 = R
    Exit Function
EH:
    M876 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M877() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("2.5")))
    M877 = R
    Exit Function
EH:
    M877 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M878() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("-2.5"))) & Dump(fV(CLng("-2.5")))
    M878 = R
    Exit Function
EH:
    M878 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M879() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("-2.5"))) & Dump(fV(CCur("-2.5")))
    M879 = R
    Exit Function
EH:
    M879 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M880() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("-2.5"))) & Dump(fV(CSng("-2.5")))
    M880 = R
    Exit Function
EH:
    M880 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M881() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("-2.5")))
    M881 = R
    Exit Function
EH:
    M881 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M882() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("-2.5")))
    M882 = R
    Exit Function
EH:
    M882 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M883() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("32767.5"))) & Dump(fV(CLng("32767.5")))
    M883 = R
    Exit Function
EH:
    M883 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M884() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("32767.5"))) & Dump(fV(CCur("32767.5")))
    M884 = R
    Exit Function
EH:
    M884 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M885() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("32767.5"))) & Dump(fV(CSng("32767.5")))
    M885 = R
    Exit Function
EH:
    M885 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M886() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("32767.5")))
    M886 = R
    Exit Function
EH:
    M886 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M887() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("32767.5")))
    M887 = R
    Exit Function
EH:
    M887 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M888() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("32768"))) & Dump(fV(CLng("32768")))
    M888 = R
    Exit Function
EH:
    M888 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M889() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("32768"))) & Dump(fV(CCur("32768")))
    M889 = R
    Exit Function
EH:
    M889 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M890() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("32768"))) & Dump(fV(CSng("32768")))
    M890 = R
    Exit Function
EH:
    M890 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M891() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("32768")))
    M891 = R
    Exit Function
EH:
    M891 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M892() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("32768")))
    M892 = R
    Exit Function
EH:
    M892 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M893() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("2147483647.5"))) & Dump(fV(CLng("2147483647.5")))
    M893 = R
    Exit Function
EH:
    M893 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M894() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("2147483647.5"))) & Dump(fV(CCur("2147483647.5")))
    M894 = R
    Exit Function
EH:
    M894 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M895() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("2147483647.5"))) & Dump(fV(CSng("2147483647.5")))
    M895 = R
    Exit Function
EH:
    M895 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M896() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("2147483647.5")))
    M896 = R
    Exit Function
EH:
    M896 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M897() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("2147483647.5")))
    M897 = R
    Exit Function
EH:
    M897 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M898() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("1E3"))) & Dump(fV(CLng("1E3")))
    M898 = R
    Exit Function
EH:
    M898 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M899() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("1E3"))) & Dump(fV(CCur("1E3")))
    M899 = R
    Exit Function
EH:
    M899 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M900() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("1E3"))) & Dump(fV(CSng("1E3")))
    M900 = R
    Exit Function
EH:
    M900 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M901() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1E3")))
    M901 = R
    Exit Function
EH:
    M901 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M902() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("1E3")))
    M902 = R
    Exit Function
EH:
    M902 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M903() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("&HFFFF"))) & Dump(fV(CLng("&HFFFF")))
    M903 = R
    Exit Function
EH:
    M903 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M904() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("&HFFFF"))) & Dump(fV(CCur("&HFFFF")))
    M904 = R
    Exit Function
EH:
    M904 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M905() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("&HFFFF"))) & Dump(fV(CSng("&HFFFF")))
    M905 = R
    Exit Function
EH:
    M905 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M906() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("&HFFFF")))
    M906 = R
    Exit Function
EH:
    M906 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M907() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("&HFFFF")))
    M907 = R
    Exit Function
EH:
    M907 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M908() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("$1,234.5"))) & Dump(fV(CLng("$1,234.5")))
    M908 = R
    Exit Function
EH:
    M908 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M909() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("$1,234.5"))) & Dump(fV(CCur("$1,234.5")))
    M909 = R
    Exit Function
EH:
    M909 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M910() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("$1,234.5"))) & Dump(fV(CSng("$1,234.5")))
    M910 = R
    Exit Function
EH:
    M910 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M911() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("$1,234.5")))
    M911 = R
    Exit Function
EH:
    M911 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M912() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("$1,234.5")))
    M912 = R
    Exit Function
EH:
    M912 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M913() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("(5)"))) & Dump(fV(CLng("(5)")))
    M913 = R
    Exit Function
EH:
    M913 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M914() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("(5)"))) & Dump(fV(CCur("(5)")))
    M914 = R
    Exit Function
EH:
    M914 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M915() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("(5)"))) & Dump(fV(CSng("(5)")))
    M915 = R
    Exit Function
EH:
    M915 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M916() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("(5)")))
    M916 = R
    Exit Function
EH:
    M916 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M917() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("(5)")))
    M917 = R
    Exit Function
EH:
    M917 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M918() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("5-"))) & Dump(fV(CLng("5-")))
    M918 = R
    Exit Function
EH:
    M918 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M919() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("5-"))) & Dump(fV(CCur("5-")))
    M919 = R
    Exit Function
EH:
    M919 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M920() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("5-"))) & Dump(fV(CSng("5-")))
    M920 = R
    Exit Function
EH:
    M920 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M921() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("5-")))
    M921 = R
    Exit Function
EH:
    M921 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M922() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("5-")))
    M922 = R
    Exit Function
EH:
    M922 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M923() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("1 000"))) & Dump(fV(CLng("1 000")))
    M923 = R
    Exit Function
EH:
    M923 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M924() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("1 000"))) & Dump(fV(CCur("1 000")))
    M924 = R
    Exit Function
EH:
    M924 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M925() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("1 000"))) & Dump(fV(CSng("1 000")))
    M925 = R
    Exit Function
EH:
    M925 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M926() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1 000")))
    M926 = R
    Exit Function
EH:
    M926 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M927() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("1 000")))
    M927 = R
    Exit Function
EH:
    M927 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M928() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("True"))) & Dump(fV(CLng("True")))
    M928 = R
    Exit Function
EH:
    M928 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M929() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("True"))) & Dump(fV(CCur("True")))
    M929 = R
    Exit Function
EH:
    M929 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M930() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("True"))) & Dump(fV(CSng("True")))
    M930 = R
    Exit Function
EH:
    M930 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M931() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("True")))
    M931 = R
    Exit Function
EH:
    M931 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M932() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("True")))
    M932 = R
    Exit Function
EH:
    M932 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M933() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("#TRUE#"))) & Dump(fV(CLng("#TRUE#")))
    M933 = R
    Exit Function
EH:
    M933 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M934() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("#TRUE#"))) & Dump(fV(CCur("#TRUE#")))
    M934 = R
    Exit Function
EH:
    M934 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M935() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("#TRUE#"))) & Dump(fV(CSng("#TRUE#")))
    M935 = R
    Exit Function
EH:
    M935 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M936() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("#TRUE#")))
    M936 = R
    Exit Function
EH:
    M936 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M937() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("#TRUE#")))
    M937 = R
    Exit Function
EH:
    M937 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M938() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("on"))) & Dump(fV(CLng("on")))
    M938 = R
    Exit Function
EH:
    M938 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M939() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("on"))) & Dump(fV(CCur("on")))
    M939 = R
    Exit Function
EH:
    M939 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M940() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("on"))) & Dump(fV(CSng("on")))
    M940 = R
    Exit Function
EH:
    M940 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M941() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("on")))
    M941 = R
    Exit Function
EH:
    M941 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M942() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("on")))
    M942 = R
    Exit Function
EH:
    M942 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M943() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("0"))) & Dump(fV(CLng("0")))
    M943 = R
    Exit Function
EH:
    M943 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M944() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("0"))) & Dump(fV(CCur("0")))
    M944 = R
    Exit Function
EH:
    M944 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M945() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("0"))) & Dump(fV(CSng("0")))
    M945 = R
    Exit Function
EH:
    M945 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M946() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("0")))
    M946 = R
    Exit Function
EH:
    M946 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M947() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("0")))
    M947 = R
    Exit Function
EH:
    M947 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M948() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("-1"))) & Dump(fV(CLng("-1")))
    M948 = R
    Exit Function
EH:
    M948 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M949() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("-1"))) & Dump(fV(CCur("-1")))
    M949 = R
    Exit Function
EH:
    M949 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M950() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte("-1"))) & Dump(fV(CSng("-1")))
    M950 = R
    Exit Function
EH:
    M950 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M951() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("-1")))
    M951 = R
    Exit Function
EH:
    M951 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M952() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("-1")))
    M952 = R
    Exit Function
EH:
    M952 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M953() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(0)))
    M953 = R
    Exit Function
EH:
    M953 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M954() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(0)))
    M954 = R
    Exit Function
EH:
    M954 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M955() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(0)))
    M955 = R
    Exit Function
EH:
    M955 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M956() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(0)))
    M956 = R
    Exit Function
EH:
    M956 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M957() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(0)))
    M957 = R
    Exit Function
EH:
    M957 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M958() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 + 1))
    M958 = R
    Exit Function
EH:
    M958 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M959() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 & "a"))
    M959 = R
    Exit Function
EH:
    M959 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M960() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0))
    M960 = R
    Exit Function
EH:
    M960 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M961() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 0))
    M961 = R
    Exit Function
EH:
    M961 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M962() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 And 3))
    M962 = R
    Exit Function
EH:
    M962 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M963() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 = 0))
    M963 = R
    Exit Function
EH:
    M963 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M964() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 \ 2))
    M964 = R
    Exit Function
EH:
    M964 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M965() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 Mod 2))
    M965 = R
    Exit Function
EH:
    M965 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M966() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 ^ 2))
    M966 = R
    Exit Function
EH:
    M966 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M967() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(1)))
    M967 = R
    Exit Function
EH:
    M967 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M968() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(1)))
    M968 = R
    Exit Function
EH:
    M968 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M969() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(1)))
    M969 = R
    Exit Function
EH:
    M969 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M970() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(1)))
    M970 = R
    Exit Function
EH:
    M970 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M971() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(1)))
    M971 = R
    Exit Function
EH:
    M971 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M972() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 + 1))
    M972 = R
    Exit Function
EH:
    M972 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M973() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 & "a"))
    M973 = R
    Exit Function
EH:
    M973 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M974() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1))
    M974 = R
    Exit Function
EH:
    M974 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M975() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 1))
    M975 = R
    Exit Function
EH:
    M975 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M976() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 And 3))
    M976 = R
    Exit Function
EH:
    M976 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M977() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 = 0))
    M977 = R
    Exit Function
EH:
    M977 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M978() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 \ 2))
    M978 = R
    Exit Function
EH:
    M978 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M979() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 Mod 2))
    M979 = R
    Exit Function
EH:
    M979 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M980() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 ^ 2))
    M980 = R
    Exit Function
EH:
    M980 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M981() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(-1)))
    M981 = R
    Exit Function
EH:
    M981 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M982() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(-1)))
    M982 = R
    Exit Function
EH:
    M982 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M983() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(-1)))
    M983 = R
    Exit Function
EH:
    M983 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M984() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(-1)))
    M984 = R
    Exit Function
EH:
    M984 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M985() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(-1)))
    M985 = R
    Exit Function
EH:
    M985 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M986() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1 + 1))
    M986 = R
    Exit Function
EH:
    M986 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M987() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1 & "a"))
    M987 = R
    Exit Function
EH:
    M987 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M988() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(--1))
    M988 = R
    Exit Function
EH:
    M988 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M989() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not -1))
    M989 = R
    Exit Function
EH:
    M989 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M990() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1 And 3))
    M990 = R
    Exit Function
EH:
    M990 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M991() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1 = 0))
    M991 = R
    Exit Function
EH:
    M991 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M992() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1 \ 2))
    M992 = R
    Exit Function
EH:
    M992 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M993() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1 Mod 2))
    M993 = R
    Exit Function
EH:
    M993 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M994() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1 ^ 2))
    M994 = R
    Exit Function
EH:
    M994 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M995() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(5)))
    M995 = R
    Exit Function
EH:
    M995 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M996() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(5)))
    M996 = R
    Exit Function
EH:
    M996 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M997() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(5)))
    M997 = R
    Exit Function
EH:
    M997 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M998() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(5)))
    M998 = R
    Exit Function
EH:
    M998 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M999() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(5)))
    M999 = R
    Exit Function
EH:
    M999 = "#ERR " & Err.Number & " " & Err.Description
End Function
