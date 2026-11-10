Attribute VB_Name = "VbaLang"
Option Explicit

Private Type TPt
    X As Long
    Y As Double
    S As String
    A(1 To 3) As Integer
End Type
Private Type TBox
    P As TPt
    Q As TPt
    N As String * 5
End Type
Private cnt As Long
Private mArr() As Long
Public Enum Colr
    Red
    Green = 5
    Blue
    Last = -1
End Enum
Private Const K1 As Long = 7
Private Const K2 = K1 * 2 + 0.5
Private Const K3 As String = "ab" & "cd"

Private Function Opt1(Optional a As Long = 5, Optional b As String = "x", Optional c As Variant) As String
    Opt1 = a & "|" & b & "|" & IsMissing(c) & "|" & TypeName(c)
End Function
Private Function Opt2(Optional a, Optional b As Variant = 3) As String
    Opt2 = IsMissing(a) & "|" & TypeName(a) & "|" & IsMissing(b) & "|" & TypeName(b) & "|" & b
End Function
Private Function Opt3(Optional a As Long, Optional b As Double, Optional c As Boolean, Optional d As Date, Optional e As Currency) As String
    Opt3 = a & "|" & b & "|" & c & "|" & CDbl(d) & "|" & e & "|" & IsMissing(a)
End Function
Private Function PA(ParamArray p() As Variant) As String
    Dim i As Long, s As String
    s = LBound(p) & ":" & UBound(p) & ":"
    For i = LBound(p) To UBound(p)
        s = s & TypeName(p(i)) & "=" & CStr(p(i)) & ";"
    Next i
    PA = s
End Function
Private Function PA2(a As Long, ParamArray p() As Variant) As String
    PA2 = a & "/" & (UBound(p) - LBound(p) + 1) & "/" & IsArray(p)
End Function
Private Sub ByRefInc(ByRef x As Long)
    x = x + 1
End Sub
Private Sub ByValInc(ByVal x As Long)
    x = x + 1
End Sub
Private Sub ByRefV(ByRef x As Variant)
    x = x & "!"
End Sub
Private Sub ByRefArr(ByRef a() As Long)
    a(1) = 99
End Sub
Private Sub ByRefD(ByRef d As Double)
    d = d + 0.5
End Sub
Private Sub SetBoth(ByRef a As Long, ByRef b As Long)
    a = 1
    b = 2
End Sub
Private Function StaticCtr() As Long
    Static n As Long
    n = n + 1
    StaticCtr = n
End Function
Private Static Function StaticFn() As Long
    Dim m As Long
    m = m + 1
    StaticFn = m
End Function
Private Function Fact(ByVal n As Long) As Double
    If n <= 1 Then Fact = 1 Else Fact = n * Fact(n - 1)
End Function
Private Function Deep(ByVal n As Long) As Long
    If n = 0 Then Deep = 0 Else Deep = 1 + Deep(n - 1)
End Function
Private Function Inf(ByVal n As Long) As Long
    Inf = Inf(n + 1) + 1
End Function
Private Function RaiseIt(ByVal n As Long) As Long
    Err.Raise n
End Function
Private Function Fails() As Long
    Dim a As Long
    a = 1 / Z()
    Fails = 5
End Function
Private Function Wrapped() As String
    On Error Resume Next
    Dim x As Long
    x = Fails()
    Wrapped = x & "|" & Err.Number
End Function
Private Function GoesUp(ByRef s As String) As Long
    On Error GoTo H
    s = s & "a"
    GoesUp = Fails()
    s = s & "b"
    Exit Function
H:
    s = s & "h" & Err.Number
    Err.Raise 9
End Function
Private Function RetArr() As Long()
    Dim r(1 To 3) As Long
    r(1) = 1: r(2) = 2: r(3) = 3
    RetArr = r
End Function
Private Function RetV() As Variant
    RetV = Array(1, "a", Null)
End Function
Private Property Get PGet() As Long
    PGet = 42
End Property
Private Function EnumName(c As Long) As String
    Select Case c
        Case Red
            EnumName = "red"
        Case Green
            EnumName = "green"
        Case Blue
            EnumName = "blue"
        Case Else
            EnumName = "?" & c
    End Select
End Function
Private Function DoubleStatic() As String
    Static s As String
    s = s & "x"
    DoubleStatic = s
End Function
Private Function FnNoAssign() As Long
End Function
Private Function FnVNoAssign() As Variant
End Function
Private Function FnSNoAssign() As String
End Function
Private Function FnOExit(ByVal x As Long) As Long
    FnOExit = 1
    If x = 1 Then Exit Function
    FnOExit = 2
End Function

Private Function fI(x As Variant) As Integer
    fI = x
End Function
Private Function fL(x As Variant) As Long
    fL = x
End Function
Private Function fD(x As Variant) As Double
    fD = x
End Function
Private Function fS(x As Variant) As Single
    fS = x
End Function
Private Function fC(x As Variant) As Currency
    fC = x
End Function
Private Function fB(x As Variant) As Byte
    fB = x
End Function
Private Function fT(x As Variant) As Boolean
    fT = x
End Function
Private Function fV(x As Variant) As Variant
    fV = x
End Function
Private Function Z() As Long
    Z = 0
End Function

Public Function L(ByVal k As Long) As Variant
    Select Case k \ 120
    Case 0: L = LC0(k)
    Case 1: L = LC1(k)
    Case 2: L = LC2(k)
    Case 3: L = LC3(k)
    Case 4: L = LC4(k)
    Case 5: L = LC5(k)
    Case 6: L = LC6(k)
    Case 7: L = LC7(k)
    Case 8: L = LC8(k)
    Case 9: L = LC9(k)
    Case 10: L = LC10(k)
    Case 11: L = LC11(k)
    Case 12: L = LC12(k)
    Case 13: L = LC13(k)
    Case 14: L = LC14(k)
    Case 15: L = LC15(k)
    Case 16: L = LC16(k)
    End Select
End Function

Private Function LC0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 0: LC0 = CInt(fD(2.5))
    Case 1: LC0 = CInt(fD(3.5))
    Case 2: LC0 = CInt(fD(-2.5))
    Case 3: LC0 = CInt(fD(32767.5))
    Case 4: LC0 = CInt(fD(-32768.5))
    Case 5: LC0 = CInt(fD(32767.4))
    Case 6: LC0 = CLng(fD(2147483647.5))
    Case 7: LC0 = CLng(fD(2147483646.5))
    Case 8: LC0 = CLng(fD(-0.5))
    Case 9: LC0 = CByte(fD(255.5))
    Case 10: LC0 = CByte(fD(254.5))
    Case 11: LC0 = CByte(fD(-0.5))
    Case 12: LC0 = CByte(fD(-0.6))
    Case 13: LC0 = CInt(fS(0.5))
    Case 14: LC0 = CInt(fC(2.5))
    Case 15: LC0 = CInt(fC(3.5))
    Case 16: LC0 = CLng(CDec(2.5))
    Case 17: LC0 = CLng(CDec(3.5))
    Case 18: LC0 = CInt("2.5")
    Case 19: LC0 = CInt("3.5")
    Case 20: LC0 = CInt(" 12 ")
    Case 21: LC0 = CInt("1e2")
    Case 22: LC0 = CInt("&HFFFF")
    Case 23: LC0 = CInt("&H8000")
    Case 24: LC0 = CLng("&HFFFFFFFF")
    Case 25: LC0 = CLng("&H7FFFFFFF")
    Case 26: LC0 = CInt("&O17")
    Case 27: LC0 = CByte("&HFF")
    Case 28: LC0 = CByte("&H100")
    Case 29: LC0 = CDbl("1,234.5")
    Case 30: LC0 = CDbl("$1")
    Case 31: LC0 = CDbl("1e")
    Case 32: LC0 = CDbl(" 12 ")
    Case 33: LC0 = CDbl("&H10")
    Case 34: LC0 = CDbl("1d3")
    Case 35: LC0 = CDbl("1.5.2")
    Case 36: LC0 = CDbl("")
    Case 37: LC0 = CDbl("+5")
    Case 38: LC0 = CDbl("5-")
    Case 39: LC0 = CDbl("(5)")
    Case 40: LC0 = CDbl("-.5")
    Case 41: LC0 = CDbl("1,2")
    Case 42: LC0 = CDbl(".")
    Case 43: LC0 = CDbl("1 2")
    Case 44: LC0 = CDbl("0x10")
    Case 45: LC0 = CDbl("1E400")
    Case 46: LC0 = CDbl("1e-400")
    Case 47: LC0 = CDbl("5%")
    Case 48: LC0 = CBool("true")
    Case 49: LC0 = CBool("False")
    Case 50: LC0 = CBool("yes")
    Case 51: LC0 = CBool("0")
    Case 52: LC0 = CBool("2")
    Case 53: LC0 = CBool("")
    Case 54: LC0 = CBool("#TRUE#")
    Case 55: LC0 = CBool(fD(0.0001))
    Case 56: LC0 = CBool(Null)
    Case 57: LC0 = CBool(Empty)
    Case 58: LC0 = CStr(Null)
    Case 59: LC0 = CStr(Empty)
    Case 60: LC0 = CStr(True)
    Case 61: LC0 = CStr(fD(1E15))
    Case 62: LC0 = CStr(fD(1E14))
    Case 63: LC0 = CStr(fD(123456789012345678#))
    Case 64: LC0 = CStr(fD(0.000001))
    Case 65: LC0 = CStr(fD(0.0001))
    Case 66: LC0 = CStr(fD(-0#))
    Case 67: LC0 = CStr(fD(1) / 3)
    Case 68: LC0 = CStr(fS(1) / 3)
    Case 69: LC0 = CStr(fS(16777216))
    Case 70: LC0 = CStr(fS(1E10))
    Case 71: LC0 = CStr(fS(1E-5))
    Case 72: LC0 = CStr(fC(123.4567))
    Case 73: LC0 = CStr(CDec(1) / 3)
    Case 74: LC0 = CStr(fD(2) ^ 0.5)
    Case 75: LC0 = CStr(fD(1E-5))
    Case 76: LC0 = CStr(fD(100000))
    Case 77: LC0 = CStr(fD(1E21))
    Case 78: LC0 = CStr(fD(123456.7890123456789))
    Case 79: LC0 = CStr(fD(0.1) + 0.2)
    Case 80: LC0 = CStr(fD(5E-324))
    Case 81: LC0 = CStr(fD(1.7976931348623157E+308))
    Case 82: LC0 = CStr(fD(2.5E-5))
    Case 83: LC0 = CStr(fD(3.00000000000001))
    Case 84: LC0 = CStr(fD(1234567890123456#))
    Case 85: LC0 = CCur(fD(0.00005))
    Case 86: LC0 = CCur(fD(0.00015))
    Case 87: LC0 = CCur(fD(0.00025))
    Case 88: LC0 = CCur(fD(922337203685477.58))
    Case 89: LC0 = CCur(fD(922337203685477.6))
    Case 90: LC0 = CCur("1,234.56789")
    Case 91: LC0 = CCur(fD(-0.00005))
    Case 92: LC0 = CSng(fD(0.1))
    Case 93: LC0 = CSng(fD(3.4028235E+38))
    Case 94: LC0 = CSng(fD(3.5E+38))
    Case 95: LC0 = CSng(fD(1E-46))
    Case 96: LC0 = CSng(fD(1.5E-45))
    Case 97: LC0 = CSng("1e39")
    Case 98: LC0 = CDbl(CSng(0.1))
    Case 99: LC0 = CDec(fD(0.1))
    Case 100: LC0 = CDec(fD(1E-30))
    Case 101: LC0 = CDec(fD(1E28))
    Case 102: LC0 = CDec(fD(7.9228162514264338E+28))
    Case 103: LC0 = CDec(fD(7.9228162514264E+28))
    Case 104: LC0 = CDec("1.2345678901234567890123456789")
    Case 105: LC0 = CDec("79228162514264337593543950336")
    Case 106: LC0 = CDec(fD(123456789.123456789))
    Case 107: LC0 = CDec(fS(0.1))
    Case 108: LC0 = CDec(fC(0.1))
    Case 109: LC0 = CDec(Null)
    Case 110: LC0 = CDec(Empty)
    Case 111: LC0 = CDec(True)
    Case 112: LC0 = CDec(fD(-1.5E-5))
    Case 113: LC0 = CVar(fB(3))
    Case 114: LC0 = TypeName(CVar(fB(3)))
    Case 115: LC0 = CLng(CVErr(2042))
    Case 116: LC0 = CInt(CVErr(2042))
    Case 117: LC0 = CStr(CVErr(2042))
    Case 118: LC0 = CDbl(CVErr(7))
    Case 119: LC0 = Int(fD(-0.5))
    End Select
    Exit Function
EH:
    LC0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 120: LC1 = Int(fD(-1E-300))
    Case 121: LC1 = Fix(fD(-2.5))
    Case 122: LC1 = Fix(fD(2.9999999999999996))
    Case 123: LC1 = Int(fC(-2.5))
    Case 124: LC1 = Int(CDec(-2.5))
    Case 125: LC1 = Fix(CDec(-2.5))
    Case 126: LC1 = Int("2.5")
    Case 127: LC1 = Int(Null)
    Case 128: LC1 = Int(Empty)
    Case 129: LC1 = Int(True)
    Case 130: LC1 = Int("a")
    Case 131: LC1 = TypeName(Int(fI(3)))
    Case 132: LC1 = TypeName(Fix(fS(3.5)))
    Case 133: LC1 = TypeName(Int(fB(3)))
    Case 134: LC1 = TypeName(Int(fC(3.5)))
    Case 135: LC1 = TypeName(Int(True))
    Case 136: LC1 = TypeName(Int("3.5"))
    Case 137: LC1 = Abs(fI(-32768))
    Case 138: LC1 = Abs(fL(-2147483647 - 1))
    Case 139: LC1 = Abs(CVar(fI(-32768)))
    Case 140: LC1 = TypeName(Abs(CVar(fI(-32768))))
    Case 141: LC1 = TypeName(Abs(CVar(fL(-2147483647) - 1)))
    Case 142: LC1 = Abs(fC(-1.5))
    Case 143: LC1 = TypeName(Abs(True))
    Case 144: LC1 = Abs("-3")
    Case 145: LC1 = TypeName(Abs("-3"))
    Case 146: LC1 = Abs(Null)
    Case 147: LC1 = Abs(Empty)
    Case 148: LC1 = TypeName(Abs(Empty))
    Case 149: LC1 = Sgn(fD(-0#))
    Case 150: LC1 = Sgn(fD(-1E-300))
    Case 151: LC1 = Sgn("-3")
    Case 152: LC1 = TypeName(Sgn(fD(3)))
    Case 153: LC1 = Sgn(Null)
    Case 154: LC1 = Sgn(True)
    Case 155: LC1 = Sgn(CDec(-1E-20))
    Case 156: LC1 = fL(5) \ 2
    Case 157: LC1 = fL(-5) \ 2
    Case 158: LC1 = fL(5) Mod fL(-3)
    Case 159: LC1 = fL(-5) Mod 3
    Case 160: LC1 = fD(5.5) Mod 2
    Case 161: LC1 = fD(4.5) Mod 2
    Case 162: LC1 = fD(2.5) \ 1
    Case 163: LC1 = fD(3.5) \ 1
    Case 164: LC1 = fD(-2.5) \ 1
    Case 165: LC1 = fL(1) \ Z()
    Case 166: LC1 = fL(1) Mod Z()
    Case 167: LC1 = fD(2147483648#) \ 1
    Case 168: LC1 = fD(2147483647.4) \ 1
    Case 169: LC1 = fD(2147483647.5) \ 1
    Case 170: LC1 = fD(1E10) Mod 3
    Case 171: LC1 = fC(7.5) \ 2
    Case 172: LC1 = fC(7.5) Mod 2
    Case 173: LC1 = CDec(7.5) \ 2
    Case 174: LC1 = CDec(7.5) Mod 2
    Case 175: LC1 = "7" \ "2"
    Case 176: LC1 = "7.5" \ 2
    Case 177: LC1 = Null \ 2
    Case 178: LC1 = Empty \ 2
    Case 179: LC1 = fL(7) \ Null
    Case 180: LC1 = True \ 2
    Case 181: LC1 = TypeName(fI(7) \ fI(2))
    Case 182: LC1 = TypeName(fL(7) \ fI(2))
    Case 183: LC1 = TypeName(fB(7) \ fB(2))
    Case 184: LC1 = TypeName(fD(7) \ fD(2))
    Case 185: LC1 = TypeName(fV(7) \ fV(2))
    Case 186: LC1 = TypeName(fV(7#) \ fV(2))
    Case 187: LC1 = TypeName(fI(7) Mod fI(2))
    Case 188: LC1 = TypeName(fB(7) Mod fB(2))
    Case 189: LC1 = TypeName(fT(True) \ fT(True))
    Case 190: LC1 = fT(True) Mod fT(True)
    Case 191: LC1 = fD(7) / 2
    Case 192: LC1 = TypeName(fL(6) / fL(2))
    Case 193: LC1 = TypeName(fI(6) / fI(2))
    Case 194: LC1 = TypeName(fS(6) / fS(2))
    Case 195: LC1 = TypeName(fC(6) / fC(2))
    Case 196: LC1 = TypeName(fB(6) / fB(2))
    Case 197: LC1 = TypeName(CDec(6) / CDec(2))
    Case 198: LC1 = TypeName(fV(6) / fV(2))
    Case 199: LC1 = TypeName(fV(7) / fV(2))
    Case 200: LC1 = fL(1) / Z()
    Case 201: LC1 = fD(1) / Z()
    Case 202: LC1 = fD(0) / Z()
    Case 203: LC1 = CDec(1) / Z()
    Case 204: LC1 = fC(1) / Z()
    Case 205: LC1 = fV(1) / Z()
    Case 206: LC1 = fV(0) / Z()
    Case 207: LC1 = fI(32767) + fI(1)
    Case 208: LC1 = fI(32767) * fI(2)
    Case 209: LC1 = fI(-32768) - fI(1)
    Case 210: LC1 = -fI(-32768)
    Case 211: LC1 = fL(2147483647) + fL(1)
    Case 212: LC1 = fL(65536) * fL(65536)
    Case 213: LC1 = fL(-2147483647) - fL(2)
    Case 214: LC1 = -fL(-2147483647 - 1)
    Case 215: LC1 = fB(200) + fB(100)
    Case 216: LC1 = fB(0) - fB(1)
    Case 217: LC1 = fB(16) * fB(16)
    Case 218: LC1 = -fB(1)
    Case 219: LC1 = fI(32767) + fL(1)
    Case 220: LC1 = TypeName(fI(1) + fL(1))
    Case 221: LC1 = TypeName(fB(1) + fI(1))
    Case 222: LC1 = TypeName(fB(1) + fB(1))
    Case 223: LC1 = TypeName(fI(1) + fS(1))
    Case 224: LC1 = TypeName(fL(1) + fS(1))
    Case 225: LC1 = TypeName(fS(1) + fD(1))
    Case 226: LC1 = TypeName(fL(1) + fD(1))
    Case 227: LC1 = TypeName(fC(1) + fL(1))
    Case 228: LC1 = TypeName(fC(1) + fS(1))
    Case 229: LC1 = TypeName(fC(1) + fD(1))
    Case 230: LC1 = TypeName(CDec(1) + fI(1))
    Case 231: LC1 = TypeName(CDec(1) + fS(1))
    Case 232: LC1 = TypeName(CDec(1) + fD(1))
    Case 233: LC1 = TypeName(CDec(1) + fC(1))
    Case 234: LC1 = TypeName(fT(True) + fT(True))
    Case 235: LC1 = fT(True) + fT(True)
    Case 236: LC1 = TypeName(fT(True) * fI(2))
    Case 237: LC1 = TypeName(fT(True) + fB(1))
    Case 238: LC1 = TypeName(fT(True) + fL(1))
    Case 239: LC1 = TypeName(fT(True) + fD(1))
    End Select
    Exit Function
EH:
    LC1 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC2(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 240: LC2 = -fT(True)
    Case 241: LC2 = TypeName(-fT(True))
    Case 242: LC2 = TypeName(-fB(1))
    Case 243: LC2 = TypeName(-fS(1))
    Case 244: LC2 = TypeName(-fC(1))
    Case 245: LC2 = fV(32767) + 1
    Case 246: LC2 = TypeName(fV(fI(32767)) + 1)
    Case 247: LC2 = TypeName(fV(fI(32767)) + fI(1))
    Case 248: LC2 = fV(fI(32767)) + fI(1)
    Case 249: LC2 = TypeName(fV(fI(32767)) * fI(2))
    Case 250: LC2 = TypeName(fV(2147483647) + 1)
    Case 251: LC2 = TypeName(fV(fL(65536)) * fV(fL(65536)))
    Case 252: LC2 = TypeName(fV(fB(200)) + fV(fB(100)))
    Case 253: LC2 = fV(fB(200)) + fV(fB(100))
    Case 254: LC2 = TypeName(fV(fB(1)) + fV(fB(1)))
    Case 255: LC2 = TypeName(fV(fB(0)) - fV(fB(1)))
    Case 256: LC2 = fV(fB(0)) - fV(fB(1))
    Case 257: LC2 = TypeName(-fV(fB(1)))
    Case 258: LC2 = TypeName(-fV(fI(-32768)))
    Case 259: LC2 = TypeName(-fV(fL(-2147483647 - 1)))
    Case 260: LC2 = TypeName(fV(fS(1)) + fV(fS(1)))
    Case 261: LC2 = TypeName(fV(fS(3.4E38)) * fV(fS(10)))
    Case 262: LC2 = TypeName(fV(fC(1)) + fV(fC(1)))
    Case 263: LC2 = TypeName(fV(fC(922337203685477#)) * fV(fL(100)))
    Case 264: LC2 = fV(fC(922337203685477#)) * fV(fL(100))
    Case 265: LC2 = TypeName(fV(fT(True)) + fV(fT(True)))
    Case 266: LC2 = fV(fT(True)) + fV(fT(True))
    Case 267: LC2 = TypeName(fV(CDec(1)) + 1)
    Case 268: LC2 = TypeName(CDec("79228162514264337593543950335") + fV(fI(1)))
    Case 269: LC2 = CDec("79228162514264337593543950335") + 1
    Case 270: LC2 = CDec("79228162514264337593543950335") * 2
    Case 271: LC2 = CDec(1) / 3
    Case 272: LC2 = CDec(2) / 3
    Case 273: LC2 = CDec(1) / 7 * 7
    Case 274: LC2 = CDec(0.1) + CDec(0.2)
    Case 275: LC2 = CDec(0.1) * 0.1
    Case 276: LC2 = CDec(1E-28) / 10
    Case 277: LC2 = CDec("1.5") * CDec("1.5")
    Case 278: LC2 = CDec(10) ^ 2
    Case 279: LC2 = TypeName(CDec(10) ^ 2)
    Case 280: LC2 = Sqr(CDec(2))
    Case 281: LC2 = TypeName(Sqr(CDec(2)))
    Case 282: LC2 = -CDec(1.5)
    Case 283: LC2 = CDec(5) - CDec(7)
    Case 284: LC2 = CDec(7) Mod CDec(3)
    Case 285: LC2 = fC(0.1) * 3
    Case 286: LC2 = fC(922337203685477.5807) + fC(0.0001)
    Case 287: LC2 = fC(922337203685477.5807) * fC(2)
    Case 288: LC2 = fC(1) / fC(3)
    Case 289: LC2 = fC(1) / 3
    Case 290: LC2 = fC(0.0001) * fC(0.5)
    Case 291: LC2 = fC(0.0003) * fC(0.5)
    Case 292: LC2 = fC(0.0001) / 2
    Case 293: LC2 = fC(0.0003) / 2
    Case 294: LC2 = fC(5) * fD(0.00005)
    Case 295: LC2 = fC(100) / fL(7)
    Case 296: LC2 = fC(100) / fD(7)
    Case 297: LC2 = fS(0.1) + fS(0.2)
    Case 298: LC2 = fS(0.1) * 3
    Case 299: LC2 = fS(16777216) + 1
    Case 300: LC2 = fS(0.1) + 0.2
    Case 301: LC2 = fS(1) / 3
    Case 302: LC2 = fS(3.4E38) * 10
    Case 303: LC2 = fS(1E-30) * fS(1E-30)
    Case 304: LC2 = fS(3) ^ 2
    Case 305: LC2 = TypeName(fS(3) ^ 2)
    Case 306: LC2 = fS(0.1) = 0.1
    Case 307: LC2 = fS(0.5) = 0.5
    Case 308: LC2 = CDbl(fS(0.1)) = fD(0.1)
    Case 309: LC2 = fS(0.1) < 0.1
    Case 310: LC2 = fD(0.1) > fS(0.1)
    Case 311: LC2 = -2 ^ 2
    Case 312: LC2 = fD(-2) ^ 2
    Case 313: LC2 = 2 ^ 3 ^ 2
    Case 314: LC2 = 2 ^ -1
    Case 315: LC2 = -2 ^ -2
    Case 316: LC2 = fD(0) ^ 0
    Case 317: LC2 = fD(0) ^ Z()
    Case 318: LC2 = fD(0) ^ -1
    Case 319: LC2 = fD(-8) ^ (1 / 3)
    Case 320: LC2 = fD(-8) ^ 3
    Case 321: LC2 = fD(-8) ^ 3.0000000001
    Case 322: LC2 = fL(2) ^ 31
    Case 323: LC2 = TypeName(fL(2) ^ 2)
    Case 324: LC2 = TypeName(fI(2) ^ fI(2))
    Case 325: LC2 = fD(10) ^ 308
    Case 326: LC2 = fD(10) ^ 309
    Case 327: LC2 = fD(10) ^ -324
    Case 328: LC2 = fD(2) ^ 0.5
    Case 329: LC2 = fD(1E-200) ^ 2
    Case 330: LC2 = fD(-1) ^ 0.5
    Case 331: LC2 = fD(-1) ^ 1E10
    Case 332: LC2 = fD(-1) ^ 1E20
    Case 333: LC2 = fD(-1) ^ 1E15
    Case 334: LC2 = "2" ^ "3"
    Case 335: LC2 = Null ^ 2
    Case 336: LC2 = Empty ^ 2
    Case 337: LC2 = True ^ 2
    Case 338: LC2 = (-True) ^ 2
    Case 339: LC2 = fC(2) ^ fC(3)
    Case 340: LC2 = TypeName(fC(2) ^ fC(3))
    Case 341: LC2 = fV(2) ^ fV(0.5)
    Case 342: LC2 = Not fL(5)
    Case 343: LC2 = Not True
    Case 344: LC2 = Not fI(0)
    Case 345: LC2 = Not fB(5)
    Case 346: LC2 = TypeName(Not fB(5))
    Case 347: LC2 = Not fD(2.5)
    Case 348: LC2 = Not fD(3.5)
    Case 349: LC2 = Not "5"
    Case 350: LC2 = Not "a"
    Case 351: LC2 = Not Null
    Case 352: LC2 = Not Empty
    Case 353: LC2 = TypeName(Not Empty)
    Case 354: LC2 = Not fD(2147483648#)
    Case 355: LC2 = Not fC(1.5)
    Case 356: LC2 = TypeName(Not fI(5))
    Case 357: LC2 = TypeName(Not fS(1))
    Case 358: LC2 = Not CDec(5)
    Case 359: LC2 = Not "True"
    End Select
    Exit Function
EH:
    LC2 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC3(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 360: LC3 = Not "false"
    Case 361: LC3 = fL(12) And fL(10)
    Case 362: LC3 = fL(12) Or fL(3)
    Case 363: LC3 = fL(12) Xor fL(10)
    Case 364: LC3 = fL(12) Eqv fL(10)
    Case 365: LC3 = fL(12) Imp fL(10)
    Case 366: LC3 = TypeName(fI(1) And fI(1))
    Case 367: LC3 = TypeName(fI(1) And fL(1))
    Case 368: LC3 = TypeName(fB(1) And fB(1))
    Case 369: LC3 = TypeName(fB(1) And fI(1))
    Case 370: LC3 = TypeName(fT(True) And fT(True))
    Case 371: LC3 = TypeName(fT(True) And fI(1))
    Case 372: LC3 = TypeName(fV(1) And fV(1))
    Case 373: LC3 = TypeName(fS(1) And fS(1))
    Case 374: LC3 = TypeName(fD(1) And fD(1))
    Case 375: LC3 = fD(2.5) And fD(3.5)
    Case 376: LC3 = fD(2.5) Or 0
    Case 377: LC3 = fD(1E10) And 1
    Case 378: LC3 = "3" And 1
    Case 379: LC3 = "a" And 1
    Case 380: LC3 = fT(True) And "True"
    Case 381: LC3 = fT(True) And "x"
    Case 382: LC3 = Null And False
    Case 383: LC3 = Null And True
    Case 384: LC3 = Null Or True
    Case 385: LC3 = Null Or False
    Case 386: LC3 = Null Xor True
    Case 387: LC3 = Null Eqv True
    Case 388: LC3 = Null Imp True
    Case 389: LC3 = False Imp Null
    Case 390: LC3 = Null Imp False
    Case 391: LC3 = True Imp Null
    Case 392: LC3 = Null And 0
    Case 393: LC3 = Null And 5
    Case 394: LC3 = Null Or 5
    Case 395: LC3 = Empty And Empty
    Case 396: LC3 = TypeName(Empty And Empty)
    Case 397: LC3 = TypeName(Null And Null)
    Case 398: LC3 = Empty Or True
    Case 399: LC3 = fT(False) Imp fT(False)
    Case 400: LC3 = fT(True) Eqv fT(True)
    Case 401: LC3 = fT(True) Xor fT(True)
    Case 402: LC3 = fT(True) Imp fT(False)
    Case 403: LC3 = fL(1) And Z()
    Case 404: LC3 = "a" < "B"
    Case 405: LC3 = "a" = "A"
    Case 406: LC3 = "10" < "9"
    Case 407: LC3 = fV(10) < fV("9")
    Case 408: LC3 = fV("10") < fV(9)
    Case 409: LC3 = fV("abc") < fV(9)
    Case 410: LC3 = fV("abc") > fV(9)
    Case 411: LC3 = fV("abc") = fV(0)
    Case 412: LC3 = fV(9) < fV("abc")
    Case 413: LC3 = fV("") < fV(0)
    Case 414: LC3 = fL(10) < "9"
    Case 415: LC3 = fL(10) = "10"
    Case 416: LC3 = fL(10) = "abc"
    Case 417: LC3 = fD(1.5) = "1.5"
    Case 418: LC3 = fV(Empty) = 0
    Case 419: LC3 = fV(Empty) = ""
    Case 420: LC3 = fV(Empty) = False
    Case 421: LC3 = Empty = Empty
    Case 422: LC3 = Null = Null
    Case 423: LC3 = Null = 1
    Case 424: LC3 = Null <> 1
    Case 425: LC3 = Null < 1
    Case 426: LC3 = Null Like "a"
    Case 427: LC3 = IsNull(Null = Null)
    Case 428: LC3 = fV(True) = fV(-1)
    Case 429: LC3 = fV(True) = fV("True")
    Case 430: LC3 = fV(True) < fV(False)
    Case 431: LC3 = fT(True) < fT(False)
    Case 432: LC3 = fT(True) = fL(-1)
    Case 433: LC3 = fV(True) = fV(1)
    Case 434: LC3 = fV(fB(1)) = fV(fT(True))
    Case 435: LC3 = fV(CDec(1)) = fV(1)
    Case 436: LC3 = fV(fC(1.5)) = fV(1.5)
    Case 437: LC3 = fV(fS(0.1)) = fV(0.1)
    Case 438: LC3 = fV(fS(0.5)) = fV(0.5)
    Case 439: LC3 = fV(#1/1/2020#) = fV(43831)
    Case 440: LC3 = fV(#1/1/2020#) = fV("1/01/2020")
    Case 441: LC3 = fV("1/1/2020") = fV(#1/1/2020#)
    Case 442: LC3 = fV("a") = fV("a")
    Case 443: LC3 = "" < "a"
    Case 444: LC3 = "a" < "aa"
    Case 445: LC3 = "abc" < "abd"
    Case 446: LC3 = "Z" < "a"
    Case 447: LC3 = "" & ChrW(233) & "" > "z"
    Case 448: LC3 = "a" Like "A"
    Case 449: LC3 = StrComp("a", "A")
    Case 450: LC3 = StrComp("a", "A", 1)
    Case 451: LC3 = StrComp("a", "b")
    Case 452: LC3 = StrComp("b", "a")
    Case 453: LC3 = StrComp(Null, "a")
    Case 454: LC3 = StrComp("a", "a", 5)
    Case 455: LC3 = StrComp("", "")
    Case 456: LC3 = StrComp(1, "1")
    Case 457: LC3 = StrComp("" & ChrW(233) & "", "e", 1)
    Case 458: LC3 = StrComp("" & ChrW(223) & "", "ss", 1)
    Case 459: LC3 = 1 & 2
    Case 460: LC3 = TypeName(1 & 2)
    Case 461: LC3 = Null & "a"
    Case 462: LC3 = TypeName(Null & Null)
    Case 463: LC3 = TypeName(Empty & Empty)
    Case 464: LC3 = TypeName(Null & Empty)
    Case 465: LC3 = TypeName(fV(1) & fV(2))
    Case 466: LC3 = True & ""
    Case 467: LC3 = fD(1.5) & "x"
    Case 468: LC3 = fD(1E15) & ""
    Case 469: LC3 = fS(0.1) & ""
    Case 470: LC3 = fC(1.5) & ""
    Case 471: LC3 = #1/2/2020# & ""
    Case 472: LC3 = #13:05:00# & ""
    Case 473: LC3 = #1/2/2020 13:05:00# & ""
    Case 474: LC3 = CDate(0) & ""
    Case 475: LC3 = CDate(0.5) & ""
    Case 476: LC3 = CDate(-1.5) & ""
    Case 477: LC3 = CVErr(7) & ""
    Case 478: LC3 = "a" + 1
    Case 479: LC3 = "1" + 1
    End Select
    Exit Function
EH:
    LC3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC4(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 480: LC4 = "1" + "1"
    Case 481: LC4 = "1" + fV(1)
    Case 482: LC4 = fV("1") + fV(1)
    Case 483: LC4 = fV("1") + fV("1")
    Case 484: LC4 = TypeName(fV("1") + 1)
    Case 485: LC4 = TypeName("1" + fI(1))
    Case 486: LC4 = TypeName("1.5" + 1)
    Case 487: LC4 = TypeName("1" + fV(1))
    Case 488: LC4 = "" + 1
    Case 489: LC4 = Empty + "a"
    Case 490: LC4 = Empty + Empty
    Case 491: LC4 = TypeName(Empty + Empty)
    Case 492: LC4 = TypeName(Empty + fI(1))
    Case 493: LC4 = TypeName(Empty * fD(1))
    Case 494: LC4 = TypeName(Empty - fB(1))
    Case 495: LC4 = Null + 1
    Case 496: LC4 = Null + "a"
    Case 497: LC4 = TypeName(Null + 1)
    Case 498: LC4 = Null * 0
    Case 499: LC4 = True + "1"
    Case 500: LC4 = "True" + 1
    Case 501: LC4 = fT(True) + "1"
    Case 502: LC4 = fV("&H10") + 1
    Case 503: LC4 = fV("1e2") + 1
    Case 504: LC4 = fV(" 5 ") + 1
    Case 505: LC4 = fV("1,000") + 1
    Case 506: LC4 = fV("$5") + 1
    Case 507: LC4 = fV("5%") + 1
    Case 508: LC4 = fV("1/2") + 1
    Case 509: LC4 = "3" * "4"
    Case 510: LC4 = "3" - "4"
    Case 511: LC4 = TypeName("3" * "4")
    Case 512: LC4 = "3" / "4"
    Case 513: LC4 = fV("3") * fV(2)
    Case 514: LC4 = fV("3") - 1
    Case 515: LC4 = -fV("3")
    Case 516: LC4 = TypeName(-fV("3"))
    Case 517: LC4 = -"3"
    Case 518: LC4 = -"a"
    Case 519: LC4 = -Null
    Case 520: LC4 = -Empty
    Case 521: LC4 = TypeName(-Empty)
    Case 522: LC4 = +"3"
    Case 523: LC4 = Val("&H10")
    Case 524: LC4 = Val(" 12abc")
    Case 525: LC4 = Val("1e3")
    Case 526: LC4 = Val("1d3")
    Case 527: LC4 = Val("&O17")
    Case 528: LC4 = Val("1,5")
    Case 529: LC4 = Val("")
    Case 530: LC4 = Val("--1")
    Case 531: LC4 = Val("-.5")
    Case 532: LC4 = Val("&HFFFF")
    Case 533: LC4 = Val("&HFFFFFFFF")
    Case 534: LC4 = Val("&H")
    Case 535: LC4 = Val("1 2")
    Case 536: LC4 = Val(" 1 2 3 ")
    Case 537: LC4 = Val("1e")
    Case 538: LC4 = Val("1e+")
    Case 539: LC4 = Val("0x10")
    Case 540: LC4 = Val("1.5.5")
    Case 541: LC4 = Val(Null)
    Case 542: LC4 = Val(5)
    Case 543: LC4 = TypeName(Val("5"))
    Case 544: LC4 = Val(True)
    Case 545: LC4 = Val("True")
    Case 546: LC4 = Val("&H1G")
    Case 547: LC4 = Val("12345678901234567890")
    Case 548: LC4 = Val("1e400")
    Case 549: LC4 = Val(".e1")
    Case 550: LC4 = Val("+5")
    Case 551: LC4 = Val("&B101")
    Case 552: LC4 = Val(" &H10")
    Case 553: LC4 = Val("1" & vbTab & "2")
    Case 554: LC4 = Val("1" & vbCr & "2")
    Case 555: LC4 = Val(ChrW(160) & "5")
    Case 556: LC4 = Str(5)
    Case 557: LC4 = Str(-5)
    Case 558: LC4 = Str(fD(1) / 3)
    Case 559: LC4 = Str(True)
    Case 560: LC4 = Str(Null)
    Case 561: LC4 = Str("5")
    Case 562: LC4 = Str(fD(1E20))
    Case 563: LC4 = Str(fD(1E-10))
    Case 564: LC4 = Str(fS(0.1))
    Case 565: LC4 = Str(fC(1.5))
    Case 566: LC4 = Str(0)
    Case 567: LC4 = Str(Empty)
    Case 568: LC4 = Str(CDec(1.50))
    Case 569: LC4 = Str(#1/1/2020#)
    Case 570: LC4 = Str(fD(1E15))
    Case 571: LC4 = Str(fD(-1.5E-7))
    Case 572: LC4 = Hex(-1)
    Case 573: LC4 = Hex(255.5)
    Case 574: LC4 = Hex(254.5)
    Case 575: LC4 = Oct(8)
    Case 576: LC4 = Hex(Null)
    Case 577: LC4 = Hex("")
    Case 578: LC4 = Hex("12")
    Case 579: LC4 = Hex(fD(2147483648#))
    Case 580: LC4 = Hex(fD(4294967295#))
    Case 581: LC4 = Hex(fD(-2147483648#))
    Case 582: LC4 = Hex(fD(-2147483649#))
    Case 583: LC4 = Hex(-32768)
    Case 584: LC4 = Hex(fB(255))
    Case 585: LC4 = Hex(fI(-1))
    Case 586: LC4 = Hex(fL(-1))
    Case 587: LC4 = Hex(Empty)
    Case 588: LC4 = Hex(True)
    Case 589: LC4 = Hex(fC(15.5))
    Case 590: LC4 = Hex(CDec(255))
    Case 591: LC4 = Oct(fI(-1))
    Case 592: LC4 = Oct(fL(-1))
    Case 593: LC4 = Oct(fB(8))
    Case 594: LC4 = Hex("a")
    Case 595: LC4 = Hex(fV(32768))
    Case 596: LC4 = Hex(fV(65535))
    Case 597: LC4 = Hex(fV(-32769))
    Case 598: LC4 = TypeName(Hex(1))
    Case 599: LC4 = Hex(0)
    End Select
    Exit Function
EH:
    LC4 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC5(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 600: LC5 = LenB("abc")
    Case 601: LC5 = LenB(fV(fI(1)))
    Case 602: LC5 = LenB(fV(1))
    Case 603: LC5 = LenB(fV(1#))
    Case 604: LC5 = LenB(fV(Null))
    Case 605: LC5 = LenB(fV(Empty))
    Case 606: LC5 = Len("" & ChrW(233) & "")
    Case 607: LC5 = Asc("")
    Case 608: LC5 = Asc("" & ChrW(233) & "")
    Case 609: LC5 = AscW("" & ChrW(233) & "")
    Case 610: LC5 = Asc("ab")
    Case 611: LC5 = Asc(Null)
    Case 612: LC5 = Asc(65)
    Case 613: LC5 = AscW(ChrW(65536 - 1))
    Case 614: LC5 = Asc("" & ChrW(8364) & "")
    Case 615: LC5 = AscW("" & ChrW(8364) & "")
    Case 616: LC5 = Chr(256)
    Case 617: LC5 = Chr(-1)
    Case 618: LC5 = Chr(255)
    Case 619: LC5 = Chr(128)
    Case 620: LC5 = Chr(0)
    Case 621: LC5 = Len(Chr(0))
    Case 622: LC5 = ChrW(65536)
    Case 623: LC5 = ChrW(-1)
    Case 624: LC5 = AscW(ChrW(-1))
    Case 625: LC5 = ChrW(-32768)
    Case 626: LC5 = ChrW(65535)
    Case 627: LC5 = Chr(65.5)
    Case 628: LC5 = Chr(66.5)
    Case 629: LC5 = Chr("65")
    Case 630: LC5 = Chr(Null)
    Case 631: LC5 = Chr(Empty)
    Case 632: LC5 = ChrW(0)
    Case 633: LC5 = TypeName(Chr(65))
    Case 634: LC5 = TypeName(Chr$(65))
    Case 635: LC5 = TypeName(Left$("a", 1))
    Case 636: LC5 = InStr(0, "abc", "b")
    Case 637: LC5 = InStr(-1, "abc", "b")
    Case 638: LC5 = InStr(5, "abc", "b")
    Case 639: LC5 = InStr(4, "abc", "")
    Case 640: LC5 = InStr(3, "abc", "")
    Case 641: LC5 = InStr("abc", "")
    Case 642: LC5 = InStr("", "")
    Case 643: LC5 = InStr("", "a")
    Case 644: LC5 = InStr(1, "abc", "B")
    Case 645: LC5 = InStr(1, "abc", "B", 1)
    Case 646: LC5 = InStr(1, "abc", "B", 2)
    Case 647: LC5 = InStr(1, "abc", "B", -1)
    Case 648: LC5 = InStr(1, "abc", "b", 3)
    Case 649: LC5 = InStr(Null, "a")
    Case 650: LC5 = InStr(1, Null, "a")
    Case 651: LC5 = InStr(1, "a", Null)
    Case 652: LC5 = InStr(2, "abcabc", "c")
    Case 653: LC5 = InStr("abc", "abcd")
    Case 654: LC5 = InStr(1.5, "abc", "b")
    Case 655: LC5 = InStr(2.5, "abc", "c")
    Case 656: LC5 = InStr(fV(2), fV("abc"), fV("c"))
    Case 657: LC5 = InStr(123, 23)
    Case 658: LC5 = InStr(1, "aXbxc", "x", vbTextCompare)
    Case 659: LC5 = InStr(1, "a" & vbNullChar & "b", vbNullChar)
    Case 660: LC5 = InStr(1, "abc", "b", 0)
    Case 661: LC5 = TypeName(InStr(Null, "a"))
    Case 662: LC5 = InStr(2147483647, "abc", "c")
    Case 663: LC5 = InStr(1, "abc", "bc", 1)
    Case 664: LC5 = InStrRev("abcabc", "b")
    Case 665: LC5 = InStrRev("abcabc", "b", -1)
    Case 666: LC5 = InStrRev("abcabc", "b", 3)
    Case 667: LC5 = InStrRev("abcabc", "b", 2)
    Case 668: LC5 = InStrRev("abcabc", "b", 0)
    Case 669: LC5 = InStrRev("abcabc", "b", 100)
    Case 670: LC5 = InStrRev("abc", "")
    Case 671: LC5 = InStrRev("abc", "", 2)
    Case 672: LC5 = InStrRev("", "a")
    Case 673: LC5 = InStrRev("abc", "B", -1, 1)
    Case 674: LC5 = InStrRev(Null, "a")
    Case 675: LC5 = InStrRev("abc", Null)
    Case 676: LC5 = InStrRev("abc", "c", -2)
    Case 677: LC5 = InStrRev("abcabc", "bc", 5)
    Case 678: LC5 = InStrRev("abcabc", "bc", 4)
    Case 679: LC5 = Mid("abc", 0)
    Case 680: LC5 = Mid("abc", 4)
    Case 681: LC5 = Mid("abc", 2, -1)
    Case 682: LC5 = Mid("abc", 2, 100)
    Case 683: LC5 = Mid("abc", 2, 0)
    Case 684: LC5 = Mid("abc", -1)
    Case 685: LC5 = Mid("abc", 1.5, 1)
    Case 686: LC5 = Mid("abc", 2.5, 1)
    Case 687: LC5 = Mid("abc", 1, 1.5)
    Case 688: LC5 = Mid("abc", 1, 2.5)
    Case 689: LC5 = Mid(Null, 1)
    Case 690: LC5 = Mid("abc", Null)
    Case 691: LC5 = Mid(12345, 2, 3)
    Case 692: LC5 = TypeName(Mid(12345, 2, 3))
    Case 693: LC5 = TypeName(Mid(Null, 1))
    Case 694: LC5 = Mid("abc", 2147483647)
    Case 695: LC5 = Mid("abc", 1, 2147483647)
    Case 696: LC5 = Mid("", 1)
    Case 697: LC5 = Mid("abc", 3, 1)
    Case 698: LC5 = Mid("abc", fV(2))
    Case 699: LC5 = Mid$("abc", 2)
    Case 700: LC5 = Mid("abc", "2")
    Case 701: LC5 = Mid("abc", "x")
    Case 702: LC5 = Mid("abc", True)
    Case 703: LC5 = Left("abc", -1)
    Case 704: LC5 = Left("abc", 0)
    Case 705: LC5 = Left("abc", 10)
    Case 706: LC5 = Left(Null, 1)
    Case 707: LC5 = Left("abc", 1.5)
    Case 708: LC5 = Left("abc", 2.5)
    Case 709: LC5 = Left(123, 2)
    Case 710: LC5 = Left("abc", Null)
    Case 711: LC5 = Right("abc", -1)
    Case 712: LC5 = Right("abc", 10)
    Case 713: LC5 = Right("abc", 1.5)
    Case 714: LC5 = Right("abc", 2.5)
    Case 715: LC5 = Right(Null, 1)
    Case 716: LC5 = Right(1234, 2)
    Case 717: LC5 = TypeName(Right(1234, 2))
    Case 718: LC5 = TypeName(Left(Null, 1))
    Case 719: LC5 = Left$(Null, 1)
    End Select
    Exit Function
EH:
    LC5 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC6(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 720: LC6 = Left("abc", 2147483647)
    Case 721: LC6 = Space(-1)
    Case 722: LC6 = Space(3)
    Case 723: LC6 = Space(2.5)
    Case 724: LC6 = Space(3.5)
    Case 725: LC6 = Space(Null)
    Case 726: LC6 = Len(Space(0))
    Case 727: LC6 = String(3, "ab")
    Case 728: LC6 = String(3, 65)
    Case 729: LC6 = String(0, "a")
    Case 730: LC6 = String(-1, "a")
    Case 731: LC6 = String(3, "")
    Case 732: LC6 = String(2, 256)
    Case 733: LC6 = String(2, -1)
    Case 734: LC6 = String(2, 65536)
    Case 735: LC6 = String(2, 65.5)
    Case 736: LC6 = String(2, "" & ChrW(233) & "")
    Case 737: LC6 = String(2.5, "a")
    Case 738: LC6 = String(Null, "a")
    Case 739: LC6 = String(2, Null)
    Case 740: LC6 = String(2, 255)
    Case 741: LC6 = String(3, 12345)
    Case 742: LC6 = String(2, 128)
    Case 743: LC6 = UCase(Null)
    Case 744: LC6 = UCase("a" & ChrW(233) & " " & ChrW(223) & "")
    Case 745: LC6 = LCase("" & ChrW(192) & "" & ChrW(201) & " " & ChrW(7838) & "")
    Case 746: LC6 = UCase(123)
    Case 747: LC6 = TypeName(UCase(Null))
    Case 748: LC6 = TypeName(UCase(1))
    Case 749: LC6 = UCase("" & ChrW(454) & "")
    Case 750: LC6 = LCase("" & ChrW(304) & "")
    Case 751: LC6 = UCase("" & ChrW(305) & "")
    Case 752: LC6 = UCase("" & ChrW(-1279) & "")
    Case 753: LC6 = Trim("  a  b  ")
    Case 754: LC6 = LTrim("  a ")
    Case 755: LC6 = RTrim(" a  ")
    Case 756: LC6 = Trim(vbTab & "a" & vbTab)
    Case 757: LC6 = Trim(ChrW(160) & "a")
    Case 758: LC6 = Trim(Null)
    Case 759: LC6 = Trim(123)
    Case 760: LC6 = Trim(Empty)
    Case 761: LC6 = TypeName(Trim(Null))
    Case 762: LC6 = Trim(vbCr & "a")
    Case 763: LC6 = Len(Trim(ChrW(12288)))
    Case 764: LC6 = StrReverse("abc")
    Case 765: LC6 = StrReverse(Null)
    Case 766: LC6 = StrReverse(123)
    Case 767: LC6 = StrReverse("")
    Case 768: LC6 = StrReverse("a" & ChrW(&HD83D) & ChrW(&HDE00))
    Case 769: LC6 = StrConv("hello wORLD", vbProperCase)
    Case 770: LC6 = StrConv("hello", vbUpperCase)
    Case 771: LC6 = StrConv("HELLO", vbLowerCase)
    Case 772: LC6 = StrConv("hello-world oneil 3rd", 3)
    Case 773: LC6 = StrConv(Null, 1)
    Case 774: LC6 = StrConv("abc", 0)
    Case 775: LC6 = StrConv("abc", 7)
    Case 776: LC6 = StrConv("abc", 16)
    Case 777: LC6 = StrConv("abc", 1 + 3)
    Case 778: LC6 = StrConv("abc", 99)
    Case 779: LC6 = StrConv(123, 1)
    Case 780: LC6 = StrConv("" & ChrW(-191) & "" & ChrW(-190) & "" & ChrW(-189) & "", 8)
    Case 781: LC6 = StrConv("abc", 4)
    Case 782: LC6 = StrConv("" & ChrW(-143) & "" & ChrW(-142) & "" & ChrW(-141) & "", 32)
    Case 783: LC6 = StrConv("a b", 3)
    Case 784: LC6 = StrConv("A.B,c", 3)
    Case 785: LC6 = StrConv("aBC dEF", 3)
    Case 786: LC6 = StrConv(" ab", 3)
    Case 787: LC6 = StrConv("ab" & vbTab & "cd", 3)
    Case 788: LC6 = StrConv("ab_cd", 3)
    Case 789: LC6 = StrConv("ab1cd", 3)
    Case 790: LC6 = StrConv("" & ChrW(233) & " " & ChrW(224) & "", 3)
    Case 791: LC6 = Replace("aaa", "a", "b", 2, 1)
    Case 792: LC6 = Replace("abc", "", "x")
    Case 793: LC6 = Replace("aaa", "a", "b", -1, -1)
    Case 794: LC6 = Replace("abc", "b", "", 1, -1)
    Case 795: LC6 = Replace("abc", "b", "x", 5)
    Case 796: LC6 = Replace("abc", "b", "x", 0)
    Case 797: LC6 = Replace("abc", "B", "x", 1, -1, 1)
    Case 798: LC6 = Replace("abc", "b", "x", 1, 0)
    Case 799: LC6 = Replace("abc", "b", "x", 1, -2)
    Case 800: LC6 = Replace("aaa", "a", "bb", 2)
    Case 801: LC6 = Replace("abc", "bc", "x", 3)
    Case 802: LC6 = Replace(Null, "a", "b")
    Case 803: LC6 = Replace("a", Null, "b")
    Case 804: LC6 = Replace("a", "a", Null)
    Case 805: LC6 = Replace(123, 2, 9)
    Case 806: LC6 = TypeName(Replace(123, 2, 9))
    Case 807: LC6 = Replace("", "", "x")
    Case 808: LC6 = Replace("", "a", "x")
    Case 809: LC6 = Replace("abab", "ab", "a", 1, 1)
    Case 810: LC6 = Replace("abc", "abc", "", 1)
    Case 811: LC6 = Replace("aaaa", "aa", "b")
    Case 812: LC6 = Replace("a", "a", "b", 2)
    Case 813: LC6 = Replace("a", "a", "b", 1)
    Case 814: LC6 = Replace("abc", "c", "d", -1)
    Case 815: LC6 = Replace("abc", "c", "d", -2)
    Case 816: LC6 = Join(Split("a,b,,c", ","), "|")
    Case 817: LC6 = UBound(Split("", ","))
    Case 818: LC6 = UBound(Split("abc", ""))
    Case 819: LC6 = Join(Split("a,b,c", ",", 2), "|")
    Case 820: LC6 = Join(Split("a,b,c", ",", 1), "|")
    Case 821: LC6 = Join(Split("a,b,c", ",", 0), "|")
    Case 822: LC6 = Join(Split("a,b,c", ",", -1), "|")
    Case 823: LC6 = Join(Split("a,b,c", ",", -2), "|")
    Case 824: LC6 = Join(Split("aXbxc", "x", -1, 1), "|")
    Case 825: LC6 = Join(Split("a b  c"), "|")
    Case 826: LC6 = Join(Split("abc", "b"), "|")
    Case 827: LC6 = Join(Split("abc", "abcd"), "|")
    Case 828: LC6 = Join(Split("abc", "abc"), "|")
    Case 829: LC6 = UBound(Split("abc", "abc"))
    Case 830: LC6 = Join(Split(Null, ","), "|")
    Case 831: LC6 = Join(Split("a,b", Null), "|")
    Case 832: LC6 = Join(Split("a,b", ""), "|")
    Case 833: LC6 = Join(Split(123, 2), "|")
    Case 834: LC6 = Join(Split(",", ","), "|")
    Case 835: LC6 = LBound(Split("a,b"))
    Case 836: LC6 = TypeName(Split("a,b"))
    Case 837: LC6 = TypeName(Split("a b")(0))
    Case 838: LC6 = Join(Array(1, 2, 3))
    Case 839: LC6 = Join(Array(1, 2, 3), "")
    End Select
    Exit Function
EH:
    LC6 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC7(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 840: LC7 = Join(Array(1, Null, 3), "-")
    Case 841: LC7 = Join(Array(1, Empty, 3), "-")
    Case 842: LC7 = Join(Array(), "-")
    Case 843: LC7 = Join(Array(1.5, True, #1/1/2020#), "-")
    Case 844: LC7 = Join(Array("a", "b"), Null)
    Case 845: LC7 = Join(Array("a", "b"), 1)
    Case 846: LC7 = Join(Array(Array(1), 2), "-")
    Case 847: LC7 = Join(Array(CVErr(7), 1), "-")
    Case 848: LC7 = Join(Array("a", "b"), "xy")
    Case 849: LC7 = Join(Null)
    Case 850: LC7 = Join("a")
    Case 851: LC7 = Join(Filter(Array("ab", "AB", "cd"), "ab"), "|")
    Case 852: LC7 = Join(Filter(Array("ab", "AB", "cd"), "ab", True, 1), "|")
    Case 853: LC7 = Join(Filter(Array("ab", "AB", "cd"), "ab", False), "|")
    Case 854: LC7 = Join(Filter(Array("ab", "AB", "cd"), "ab", False, 1), "|")
    Case 855: LC7 = UBound(Filter(Array("a"), "z"))
    Case 856: LC7 = Join(Filter(Array("a", Null), "a"), "|")
    Case 857: LC7 = Join(Filter(Array(1, 22, 3), 2), "|")
    Case 858: LC7 = Join(Filter(Array("a"), ""), "|")
    Case 859: LC7 = Join(Filter(Array("a"), Null), "|")
    Case 860: LC7 = Filter(Null, "a")
    Case 861: LC7 = "abc" Like "a*c"
    Case 862: LC7 = "ac" Like "a*c"
    Case 863: LC7 = "abc" Like "a?c"
    Case 864: LC7 = "abc" Like "a[a-c]c"
    Case 865: LC7 = "abc" Like "a[!a-c]c"
    Case 866: LC7 = "a1c" Like "a#c"
    Case 867: LC7 = "a" Like "[]"
    Case 868: LC7 = "" Like "[]"
    Case 869: LC7 = "a*c" Like "a[*]c"
    Case 870: LC7 = "a]c" Like "a[]]c"
    Case 871: LC7 = "abc" Like "a[c-a]c"
    Case 872: LC7 = "b" Like "[a-]"
    Case 873: LC7 = "-" Like "[a-]"
    Case 874: LC7 = "-" Like "[-a]"
    Case 875: LC7 = "a" Like "[!]"
    Case 876: LC7 = "abc" Like "*"
    Case 877: LC7 = "" Like "*"
    Case 878: LC7 = "" Like "?"
    Case 879: LC7 = "" Like ""
    Case 880: LC7 = "a" Like ""
    Case 881: LC7 = "abc" Like "ABC"
    Case 882: LC7 = "abc" Like "[A-C][A-C][A-C]"
    Case 883: LC7 = "B" Like "[a-c]"
    Case 884: LC7 = "" & ChrW(233) & "" Like "[a-z]"
    Case 885: LC7 = "e" Like "[" & ChrW(224) & "-" & ChrW(233) & "]"
    Case 886: LC7 = "a" Like "[a"
    Case 887: LC7 = "a" Like "a["
    Case 888: LC7 = "a[" Like "a[["
    Case 889: LC7 = "!" Like "[!]"
    Case 890: LC7 = "!" Like "[!!]"
    Case 891: LC7 = "a" Like "[!!]"
    Case 892: LC7 = "5" Like "[0-9]"
    Case 893: LC7 = "x" Like "[0-9]"
    Case 894: LC7 = "a" Like "[a-a]"
    Case 895: LC7 = "ab" Like "*[b]"
    Case 896: LC7 = "a#" Like "a[#]"
    Case 897: LC7 = "a?" Like "a[?]"
    Case 898: LC7 = "abc" Like "a**c"
    Case 899: LC7 = "abc" Like "*b*"
    Case 900: LC7 = "aXb" Like "a?b"
    Case 901: LC7 = "ab" Like "a?b"
    Case 902: LC7 = 1 Like 1
    Case 903: LC7 = 12 Like "1#"
    Case 904: LC7 = Null Like "a"
    Case 905: LC7 = "a" Like Null
    Case 906: LC7 = fV("abc") Like "a*"
    Case 907: LC7 = "a" & vbCr Like "a?"
    Case 908: LC7 = "abc" Like "a[b-b]c"
    Case 909: LC7 = "a-c" Like "a[b-]c"
    Case 910: LC7 = "abbbc" Like "a*b*c"
    Case 911: LC7 = "abc" Like "*c*"
    Case 912: LC7 = "ab" Like "*c*"
    Case 913: LC7 = "a.b" Like "a.b"
    Case 914: LC7 = "a" Like "[[]"
    Case 915: LC7 = "[" Like "[[]"
    Case 916: LC7 = "a" Like "[!a-z]"
    Case 917: LC7 = "A" Like "[!a-z]"
    Case 918: LC7 = Format(1234.5, "#,##0.00")
    Case 919: LC7 = Format(0.5, "Percent")
    Case 920: LC7 = Format(-1, "0;(0)")
    Case 921: LC7 = Format(0, "0;(0);zero")
    Case 922: LC7 = Format(1, "Yes/No")
    Case 923: LC7 = Format(0, "Yes/No")
    Case 924: LC7 = Format(True, "Yes/No")
    Case 925: LC7 = Format(1, "On/Off")
    Case 926: LC7 = Format(1, "True/False")
    Case 927: LC7 = Format("abc", ">")
    Case 928: LC7 = Format("ABC", "<")
    Case 929: LC7 = Format(1234567.891, "0.00E+00")
    Case 930: LC7 = Format(12, "000")
    Case 931: LC7 = Format(0.000123, "0.0000")
    Case 932: LC7 = Format(True)
    Case 933: LC7 = Format(Null)
    Case 934: LC7 = Format(Null, "0")
    Case 935: LC7 = Format(Empty)
    Case 936: LC7 = Format(Empty, "0")
    Case 937: LC7 = Format("", "0")
    Case 938: LC7 = Format(5, "General Number")
    Case 939: LC7 = Format(1 / 3, "Fixed")
    Case 940: LC7 = Format(1234.5, "Standard")
    Case 941: LC7 = Format(1234.5, "Currency")
    Case 942: LC7 = Format(-1234.5, "Currency")
    Case 943: LC7 = Format(0.256, "Percent")
    Case 944: LC7 = Format(1234.5, "Scientific")
    Case 945: LC7 = Format(0.5, "Scientific")
    Case 946: LC7 = Format(1, "Fixed")
    Case 947: LC7 = Format(1234567, "Standard")
    Case 948: LC7 = Format(0, "Standard")
    Case 949: LC7 = Format(#1/2/2020#, "dddd mmmm d yyyy")
    Case 950: LC7 = Format(#1:05:00 PM#, "h:mm AM/PM")
    Case 951: LC7 = Format(#1:05:00 PM#, "h:mm am/pm")
    Case 952: LC7 = Format(#1:05:00 PM#, "h:mm A/P")
    Case 953: LC7 = Format(#1:05:00 PM#, "hh:mm:ss a/p")
    Case 954: LC7 = Format(#1/2/2020#, "Long Date")
    Case 955: LC7 = Format(#1/2/2020#, "Short Date")
    Case 956: LC7 = Format(#1/2/2020#, "Medium Date")
    Case 957: LC7 = Format(#1:05:09 PM#, "Long Time")
    Case 958: LC7 = Format(#1:05:09 PM#, "Medium Time")
    Case 959: LC7 = Format(#1:05:09 PM#, "Short Time")
    End Select
    Exit Function
EH:
    LC7 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC8(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 960: LC8 = Format(#1/2/2020 1:05:09 PM#, "General Date")
    Case 961: LC8 = Format(#1/2/2020#, "General Date")
    Case 962: LC8 = Format(#1:05:09 PM#, "General Date")
    Case 963: LC8 = Format(#1/2/2020#)
    Case 964: LC8 = Format(#1/2/2020#, "ddd d/m/yy")
    Case 965: LC8 = Format(#1/2/2020#, "ww")
    Case 966: LC8 = Format(#1/2/2020#, "q")
    Case 967: LC8 = Format(#1/2/2020#, "y")
    Case 968: LC8 = Format(#1/2/2020#, "w")
    Case 969: LC8 = Format(#1/2/2020#, "ddddd")
    Case 970: LC8 = Format(#1/2/2020#, "dddddd")
    Case 971: LC8 = Format(#1/2/2020#, "ttttt")
    Case 972: LC8 = Format(#12/31/2020#, "ww", 2, 2)
    Case 973: LC8 = Format(#1/1/2021#, "ww", 1, 3)
    Case 974: LC8 = Format(#1/1/2021#, "ww", 2, 2)
    Case 975: LC8 = Format(#1/2/2020#, "yyyy-mm-dd\Thh:nn:ss")
    Case 976: LC8 = Format(#1/2/2020 5:06:07#, "n/nn")
    Case 977: LC8 = Format(#1/2/2020 5:06:07#, "m")
    Case 978: LC8 = Format(#1/2/2020 5:06:07#, "h:m")
    Case 979: LC8 = Format(#1/2/2020 5:06:07#, "mm:ss")
    Case 980: LC8 = Format(1.5, "hh:mm")
    Case 981: LC8 = Format(0.999999, "hh:mm:ss")
    Case 982: LC8 = Format(0.9999999, "hh:mm:ss")
    Case 983: LC8 = Format(1.9999999, "d hh:mm:ss")
    Case 984: LC8 = Format(0.5, "AM/PM h")
    Case 985: LC8 = Format(0.5, "[h]:mm")
    Case 986: LC8 = Format(-0.5, "hh:mm")
    Case 987: LC8 = Format("1/2/2020", "yyyy")
    Case 988: LC8 = Format("12:30", "h:mm AM/PM")
    Case 989: LC8 = Format("abc", "yyyy")
    Case 990: LC8 = Format("hello", "@@@@@@@")
    Case 991: LC8 = Format("hello", "!@@@@@@@")
    Case 992: LC8 = Format("hello", "@@")
    Case 993: LC8 = Format("hello", "&&&&&&&")
    Case 994: LC8 = Format("hello", "!&&&&&&&")
    Case 995: LC8 = Format("hello", "@;x")
    Case 996: LC8 = Format("", "@;empty")
    Case 997: LC8 = Format("", ";;;empty")
    Case 998: LC8 = Format(Null, "0;0;0;nullstr")
    Case 999: LC8 = Format(12345, "##-##")
    Case 1000: LC8 = Format(12345, "#,#")
    Case 1001: LC8 = Format(0.1 + 0.2, "0.0000000000000000")
    Case 1002: LC8 = Format(0.5, "0")
    Case 1003: LC8 = Format(1.5, "0")
    Case 1004: LC8 = Format(2.5, "0")
    Case 1005: LC8 = Format(0.125, "0.00")
    Case 1006: LC8 = Format(0.135, "0.00")
    Case 1007: LC8 = Format(2.675, "0.00")
    Case 1008: LC8 = Format(1E+20, "0")
    Case 1009: LC8 = Format(123456789012345678#, "0")
    Case 1010: LC8 = Format(1E+25, "#,##0")
    Case 1011: LC8 = Format(-0.001, "0.00")
    Case 1012: LC8 = Format(-0.001, "0.00;(0.00)")
    Case 1013: LC8 = Format(0.001, "0.00;(0.00);Z")
    Case 1014: LC8 = Format(0, "0.00;(0.00);Z")
    Case 1015: LC8 = Format(5, "0.0\%")
    Case 1016: LC8 = Format(5, "0%")
    Case 1017: LC8 = Format(0.5, "#.##")
    Case 1018: LC8 = Format(0, "#.##")
    Case 1019: LC8 = Format(0, "0.##")
    Case 1020: LC8 = Format(1234.5678, "#,##0.0#")
    Case 1021: LC8 = Format(1234.5678, ".00")
    Case 1022: LC8 = Format(0.1234, "#%")
    Case 1023: LC8 = Format(1234567.891, "#,###.00")
    Case 1024: LC8 = Format(1234567.891, "#,,")
    Case 1025: LC8 = Format(1234567.891, "0.0,,")
    Case 1026: LC8 = Format(1234, "$#,##0")
    Case 1027: LC8 = Format(1234, "\$0")
    Case 1028: LC8 = Format(1234, """x""0")
    Case 1029: LC8 = Format(1234, "0 ""kg""")
    Case 1030: LC8 = Format(1, "0.0E-0")
    Case 1031: LC8 = Format(12345, "0.0E+0")
    Case 1032: LC8 = Format(0.00012345, "0.00E-00")
    Case 1033: LC8 = Format(1, "0;;;")
    Case 1034: LC8 = Format(-1, ";0")
    Case 1035: LC8 = Format(1, ";;0")
    Case 1036: LC8 = Format(1, "[Red]0")
    Case 1037: LC8 = Format(1, "[Blue]0.0;[Red]-0.0")
    Case 1038: LC8 = Format(1, "\a0")
    Case 1039: LC8 = Format(1, "*-0")
    Case 1040: LC8 = Format(1, "_(0_)")
    Case 1041: LC8 = Format(1, "0 ?/?")
    Case 1042: LC8 = Format(2.5, "# ?/?")
    Case 1043: LC8 = Format(1, "[>0]""p"";""n""")
    Case 1044: LC8 = Format(5, "#")
    Case 1045: LC8 = Format(0, "#")
    Case 1046: LC8 = Format(0, "")
    Case 1047: LC8 = Format(5, "")
    Case 1048: LC8 = Format(5)
    Case 1049: LC8 = Format(1 / 3)
    Case 1050: LC8 = Format(1E+15)
    Case 1051: LC8 = Format(0.00001)
    Case 1052: LC8 = Format(1E-10)
    Case 1053: LC8 = Format(123456789012, "General Number")
    Case 1054: LC8 = Format(fS(0.1))
    Case 1055: LC8 = Format(fS(1) / 3)
    Case 1056: LC8 = Format(fC(1.5))
    Case 1057: LC8 = Format(CDec(1.50))
    Case 1058: LC8 = Format(fB(5))
    Case 1059: LC8 = Format(fV("abc"))
    Case 1060: LC8 = Format(fV("5"), "0.0")
    Case 1061: LC8 = Format(fV("abc"), "0.0")
    Case 1062: LC8 = Format(5, "0.0", 1)
    Case 1063: LC8 = Format(CVErr(7))
    Case 1064: LC8 = Format(5, 5)
    Case 1065: LC8 = Format(5, Null)
    Case 1066: LC8 = DateSerial(2020, 2, 30)
    Case 1067: LC8 = DateSerial(2020, 14, 1)
    Case 1068: LC8 = DateSerial(100, 1, 1)
    Case 1069: LC8 = DateSerial(99, 1, 1)
    Case 1070: LC8 = DateSerial(29, 1, 1)
    Case 1071: LC8 = DateSerial(30, 1, 1)
    Case 1072: LC8 = DateSerial(0, 1, 1)
    Case 1073: LC8 = DateSerial(-1, 1, 1)
    Case 1074: LC8 = DateSerial(9999, 12, 31)
    Case 1075: LC8 = DateSerial(9999, 12, 32)
    Case 1076: LC8 = DateSerial(10000, 1, 1)
    Case 1077: LC8 = DateSerial(1900, 2, 29)
    Case 1078: LC8 = DateSerial(1900, 1, 0)
    Case 1079: LC8 = DateSerial(1900, 1, -1)
    End Select
    Exit Function
EH:
    LC8 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC9(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1080: LC9 = DateSerial(1899, 12, 30)
    Case 1081: LC9 = DateSerial(1, 1, 1)
    Case 1082: LC9 = DateSerial(2020, 0, 0)
    Case 1083: LC9 = DateSerial(2020, -1, 1)
    Case 1084: LC9 = DateSerial(2020, 1.5, 1)
    Case 1085: LC9 = DateSerial(2020, 2.5, 1)
    Case 1086: LC9 = DateSerial(2020, 1, 1.5)
    Case 1087: LC9 = DateSerial(Null, 1, 1)
    Case 1088: LC9 = DateSerial("2020", "1", "1")
    Case 1089: LC9 = DateSerial(32768, 1, 1)
    Case 1090: LC9 = DateSerial(2020, 32768, 1)
    Case 1091: LC9 = DateSerial(2020, 1, 100000)
    Case 1092: LC9 = DateSerial(100, 1, 1 - 365 * 100)
    Case 1093: LC9 = DateSerial(9999, 12, 31) + 1
    Case 1094: LC9 = TypeName(DateSerial(2020, 1, 1))
    Case 1095: LC9 = DateSerial(1, 1, -1000000)
    Case 1096: LC9 = DateSerial(2020, 1, 2147483647)
    Case 1097: LC9 = TimeSerial(25, 61, 61)
    Case 1098: LC9 = TimeSerial(-1, 0, 0)
    Case 1099: LC9 = TimeSerial(0, 0, -1)
    Case 1100: LC9 = TimeSerial(0, 0, 0)
    Case 1101: LC9 = TimeSerial(12, 30, 45.5)
    Case 1102: LC9 = TimeSerial(23, 59, 60)
    Case 1103: LC9 = TimeSerial(24, 0, 0)
    Case 1104: LC9 = TimeSerial(-25, 0, 0)
    Case 1105: LC9 = TimeSerial(0, 1440, 0)
    Case 1106: LC9 = TimeSerial(0, 0, 86400)
    Case 1107: LC9 = TimeSerial(0, 0, 100000)
    Case 1108: LC9 = TimeSerial(100, 0, 0)
    Case 1109: LC9 = TimeSerial(1000, 0, 0)
    Case 1110: LC9 = TimeSerial(32767, 32767, 32767)
    Case 1111: LC9 = TimeSerial(Null, 1, 1)
    Case 1112: LC9 = TimeSerial(12.5, 0, 0)
    Case 1113: LC9 = TimeSerial(13.5, 0, 0)
    Case 1114: LC9 = TimeSerial(1, 2, 3) - TimeSerial(1, 2, 2)
    Case 1115: LC9 = DateAdd("m", 1, #1/31/2020#)
    Case 1116: LC9 = DateAdd("yyyy", 1, #2/29/2020#)
    Case 1117: LC9 = DateAdd("q", 1, #11/30/2020#)
    Case 1118: LC9 = DateAdd("w", 3, #1/1/2020#)
    Case 1119: LC9 = DateAdd("ww", 2, #1/1/2020#)
    Case 1120: LC9 = DateAdd("y", 400, #1/1/2020#)
    Case 1121: LC9 = DateAdd("d", 1.5, #1/1/2020#)
    Case 1122: LC9 = DateAdd("d", 1.4, #1/1/2020#)
    Case 1123: LC9 = DateAdd("d", -1.5, #1/1/2020#)
    Case 1124: LC9 = DateAdd("h", 25, #1/1/2020#)
    Case 1125: LC9 = DateAdd("n", 90, #1/1/2020#)
    Case 1126: LC9 = DateAdd("s", 86399, #1/1/2020#)
    Case 1127: LC9 = DateAdd("s", 0.5, #1/1/2020#)
    Case 1128: LC9 = DateAdd("h", 0.5, #1/1/2020#)
    Case 1129: LC9 = DateAdd("m", -1, #3/31/2020#)
    Case 1130: LC9 = DateAdd("m", 12000, #1/1/2020#)
    Case 1131: LC9 = DateAdd("yyyy", 8000, #1/1/2020#)
    Case 1132: LC9 = DateAdd("yyyy", -2000, #1/1/2020#)
    Case 1133: LC9 = DateAdd("yyyy", -119, #1/1/2020#)
    Case 1134: LC9 = DateAdd("yyyy", -120, #1/1/2020#)
    Case 1135: LC9 = DateAdd("x", 1, #1/1/2020#)
    Case 1136: LC9 = DateAdd("", 1, #1/1/2020#)
    Case 1137: LC9 = DateAdd("D", 1, #1/1/2020#)
    Case 1138: LC9 = DateAdd("dd", 1, #1/1/2020#)
    Case 1139: LC9 = DateAdd(Null, 1, #1/1/2020#)
    Case 1140: LC9 = DateAdd("d", Null, #1/1/2020#)
    Case 1141: LC9 = DateAdd("d", 1, Null)
    Case 1142: LC9 = DateAdd("d", 1, "1/1/2020")
    Case 1143: LC9 = DateAdd("d", 1, "abc")
    Case 1144: LC9 = DateAdd("d", 1, 0)
    Case 1145: LC9 = DateAdd("d", 1, 43831.5)
    Case 1146: LC9 = DateAdd("m", 1, 43831.5)
    Case 1147: LC9 = DateAdd("d", 2147483647, #1/1/2020#)
    Case 1148: LC9 = DateAdd("d", 2958465, #1/1/2020#)
    Case 1149: LC9 = DateAdd("d", 1, #12/31/9999#)
    Case 1150: LC9 = DateAdd("d", -1, #1/1/0100#)
    Case 1151: LC9 = DateAdd("m", 1, #12/31/9999#)
    Case 1152: LC9 = DateAdd("yyyy", 1, #12/31/9999#)
    Case 1153: LC9 = DateAdd("yyyy", 1.5, #1/1/2020#)
    Case 1154: LC9 = DateAdd("yyyy", 2.5, #1/1/2020#)
    Case 1155: LC9 = DateAdd("m", 1, #1/30/2020#)
    Case 1156: LC9 = DateAdd("m", 1, #3/1/1900#)
    Case 1157: LC9 = DateAdd("d", 1, #2/28/1900#)
    Case 1158: LC9 = TypeName(DateAdd("d", 1, #1/1/2020#))
    Case 1159: LC9 = TypeName(DateAdd("d", 1, "1/1/2020"))
    Case 1160: LC9 = DateDiff("m", #1/31/2020#, #2/1/2020#)
    Case 1161: LC9 = DateDiff("yyyy", #12/31/2019#, #1/1/2020#)
    Case 1162: LC9 = DateDiff("q", #12/31/2019#, #1/1/2020#)
    Case 1163: LC9 = DateDiff("ww", #1/1/2020#, #1/8/2020#)
    Case 1164: LC9 = DateDiff("ww", #1/4/2020#, #1/5/2020#)
    Case 1165: LC9 = DateDiff("ww", #1/4/2020#, #1/5/2020#, vbMonday)
    Case 1166: LC9 = DateDiff("ww", #1/4/2020#, #1/5/2020#, vbSunday)
    Case 1167: LC9 = DateDiff("ww", #1/5/2020#, #1/4/2020#)
    Case 1168: LC9 = DateDiff("w", #1/1/2020#, #1/15/2020#)
    Case 1169: LC9 = DateDiff("w", #1/1/2020#, #1/14/2020#)
    Case 1170: LC9 = DateDiff("d", #1/1/2020#, #1/2/2020#)
    Case 1171: LC9 = DateDiff("d", #1/1/2020 11:59:00 PM#, #1/2/2020 12:01:00 AM#)
    Case 1172: LC9 = DateDiff("y", #1/1/2020#, #3/1/2020#)
    Case 1173: LC9 = DateDiff("h", #1/1/2020 12:59:00 AM#, #1/1/2020 1:01:00 AM#)
    Case 1174: LC9 = DateDiff("n", #1/1/2020 12:00:59 AM#, #1/1/2020 12:01:01 AM#)
    Case 1175: LC9 = DateDiff("s", #1/1/2020#, #1/1/2021#)
    Case 1176: LC9 = DateDiff("s", #1/1/1900#, #1/1/2100#)
    Case 1177: LC9 = DateDiff("s", #1/1/0100#, #12/31/9999#)
    Case 1178: LC9 = DateDiff("d", #1/1/0100#, #12/31/9999#)
    Case 1179: LC9 = DateDiff("yyyy", #1/1/0100#, #12/31/9999#)
    Case 1180: LC9 = DateDiff("m", #3/31/2020#, #2/29/2020#)
    Case 1181: LC9 = DateDiff("yyyy", #1/1/2020#, #1/1/2020#)
    Case 1182: LC9 = DateDiff("x", #1/1/2020#, #1/1/2020#)
    Case 1183: LC9 = DateDiff("d", Null, #1/1/2020#)
    Case 1184: LC9 = DateDiff("d", "1/1/2020", "1/3/2020")
    Case 1185: LC9 = DateDiff("d", 0, 1)
    Case 1186: LC9 = DateDiff("d", 0.5, 1.4)
    Case 1187: LC9 = DateDiff("d", -1.5, -0.5)
    Case 1188: LC9 = DateDiff("ww", #1/1/2020#, #1/1/2020#, 8)
    Case 1189: LC9 = DateDiff("ww", #1/1/2020#, #1/8/2020#, 0)
    Case 1190: LC9 = DateDiff("ww", #1/1/2020#, #1/8/2020#, -1)
    Case 1191: LC9 = DateDiff("ww", #1/1/2020#, #1/8/2020#, 1, 4)
    Case 1192: LC9 = DateDiff("yyyy", #12/28/2020#, #1/2/2021#, 1, 2)
    Case 1193: LC9 = DateDiff("yyyy", #12/28/2020#, #1/2/2021#, 1, 3)
    Case 1194: LC9 = DateDiff("yyyy", #12/28/2020#, #1/2/2021#, 1, 9)
    Case 1195: LC9 = TypeName(DateDiff("d", #1/1/2020#, #1/2/2020#))
    Case 1196: LC9 = DateDiff("q", #1/1/2020#, #12/31/2020#)
    Case 1197: LC9 = DateDiff("m", #1/1/2020#, #1/1/2019#)
    Case 1198: LC9 = DateDiff("h", 0, 0.99999999)
    Case 1199: LC9 = DateDiff("s", 0, 0.00001)
    End Select
    Exit Function
EH:
    LC9 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC10(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1200: LC10 = DateDiff("d", #1/1/2020#, 2958465)
    Case 1201: LC10 = DateDiff("d", #1/1/2020#, 2958466)
    Case 1202: LC10 = DatePart("ww", #1/1/2020#)
    Case 1203: LC10 = DatePart("ww", #1/1/2021#)
    Case 1204: LC10 = DatePart("ww", #1/1/2021#, vbMonday, vbFirstFourDays)
    Case 1205: LC10 = DatePart("ww", #1/1/2021#, vbMonday, vbFirstFullWeek)
    Case 1206: LC10 = DatePart("ww", #1/1/2021#, vbSunday, vbFirstJan1)
    Case 1207: LC10 = DatePart("ww", #12/31/2018#, vbMonday, vbFirstFourDays)
    Case 1208: LC10 = DatePart("ww", #12/30/2019#, vbMonday, vbFirstFourDays)
    Case 1209: LC10 = DatePart("ww", #12/31/2019#, vbMonday, vbFirstFourDays)
    Case 1210: LC10 = DatePart("ww", #1/3/2016#, vbMonday, vbFirstFourDays)
    Case 1211: LC10 = DatePart("ww", #12/31/2015#, vbMonday, vbFirstFourDays)
    Case 1212: LC10 = DatePart("ww", #12/31/2020#, vbSunday, vbFirstFullWeek)
    Case 1213: LC10 = DatePart("ww", #12/31/2020#, vbMonday, vbFirstFullWeek)
    Case 1214: LC10 = DatePart("ww", #1/1/2020#, vbUseSystemDayOfWeek, vbUseSystem)
    Case 1215: LC10 = DatePart("w", #1/1/2020#)
    Case 1216: LC10 = DatePart("w", #1/1/2020#, vbMonday)
    Case 1217: LC10 = DatePart("w", #1/1/2020#, 8)
    Case 1218: LC10 = DatePart("y", #3/1/2020#)
    Case 1219: LC10 = DatePart("y", #3/1/1900#)
    Case 1220: LC10 = DatePart("y", #12/31/2020#)
    Case 1221: LC10 = DatePart("q", #2/1/2020#)
    Case 1222: LC10 = DatePart("q", #12/31/2020#)
    Case 1223: LC10 = DatePart("yyyy", #1/1/0100#)
    Case 1224: LC10 = DatePart("yyyy", 0)
    Case 1225: LC10 = DatePart("m", 0)
    Case 1226: LC10 = DatePart("d", 0)
    Case 1227: LC10 = DatePart("h", 0.999999)
    Case 1228: LC10 = DatePart("n", 0.9999999)
    Case 1229: LC10 = DatePart("s", 0.99999999)
    Case 1230: LC10 = DatePart("x", 0)
    Case 1231: LC10 = DatePart("d", Null)
    Case 1232: LC10 = DatePart("d", "abc")
    Case 1233: LC10 = DatePart("d", "1/2/2020")
    Case 1234: LC10 = DatePart("w", -1)
    Case 1235: LC10 = DatePart("w", -6)
    Case 1236: LC10 = DatePart("w", -0.5)
    Case 1237: LC10 = DatePart("d", -1.5)
    Case 1238: LC10 = DatePart("yyyy", -700000)
    Case 1239: LC10 = DatePart("yyyy", -657434)
    Case 1240: LC10 = DatePart("yyyy", -657435)
    Case 1241: LC10 = DatePart("yyyy", 2958465)
    Case 1242: LC10 = DatePart("yyyy", 2958466)
    Case 1243: LC10 = DatePart("ww", 0)
    Case 1244: LC10 = DatePart("ww", 1)
    Case 1245: LC10 = DatePart("ww", -1)
    Case 1246: LC10 = DatePart("ww", 2)
    Case 1247: LC10 = DatePart("ww", 3)
    Case 1248: LC10 = Weekday(0)
    Case 1249: LC10 = Weekday(1)
    Case 1250: LC10 = Weekday(-1)
    Case 1251: LC10 = Weekday(-0.5)
    Case 1252: LC10 = Weekday(#1/1/2020#)
    Case 1253: LC10 = Weekday(#1/1/2020#, vbMonday)
    Case 1254: LC10 = Weekday(#1/1/2020#, 0)
    Case 1255: LC10 = Weekday(#1/1/2020#, 8)
    Case 1256: LC10 = Weekday(#1/1/2020#, -1)
    Case 1257: LC10 = Weekday(Null)
    Case 1258: LC10 = Weekday("abc")
    Case 1259: LC10 = Weekday("1/1/2020")
    Case 1260: LC10 = Weekday(#1/1/0100#)
    Case 1261: LC10 = Weekday(#12/31/9999#)
    Case 1262: LC10 = Weekday(2958465.9)
    Case 1263: LC10 = Weekday(2958466)
    Case 1264: LC10 = Month(0)
    Case 1265: LC10 = Year(0)
    Case 1266: LC10 = Day(-1)
    Case 1267: LC10 = Day(0)
    Case 1268: LC10 = Month(-1)
    Case 1269: LC10 = Year(-1)
    Case 1270: LC10 = Year(-657434)
    Case 1271: LC10 = Year(-657435)
    Case 1272: LC10 = Year(2958465)
    Case 1273: LC10 = Year(2958466)
    Case 1274: LC10 = Year(2958465.99999)
    Case 1275: LC10 = Year(Null)
    Case 1276: LC10 = Year("2020-03-04")
    Case 1277: LC10 = Year("3/4")
    Case 1278: LC10 = Day("3/4")
    Case 1279: LC10 = Month("March 5")
    Case 1280: LC10 = Day("5 March 2020")
    Case 1281: LC10 = Month(" 5 March 2020 ")
    Case 1282: LC10 = Year(1.99999999999)
    Case 1283: LC10 = Year(1E10)
    Case 1284: LC10 = Year("abc")
    Case 1285: LC10 = Year(True)
    Case 1286: LC10 = Year("")
    Case 1287: LC10 = Month(60)
    Case 1288: LC10 = Day(60)
    Case 1289: LC10 = Month(59)
    Case 1290: LC10 = Day(59)
    Case 1291: LC10 = Month(61)
    Case 1292: LC10 = Day(61)
    Case 1293: LC10 = Day(-59)
    Case 1294: LC10 = Month(-59)
    Case 1295: LC10 = Hour(0.999999)
    Case 1296: LC10 = Minute(0.9999999)
    Case 1297: LC10 = Second(0.99999999)
    Case 1298: LC10 = Second(0.5 + 1 / 86400 / 2)
    Case 1299: LC10 = Second(1 / 86400 / 2)
    Case 1300: LC10 = Second(0.5 / 86400)
    Case 1301: LC10 = Second(1.5 / 86400)
    Case 1302: LC10 = Second(2.5 / 86400)
    Case 1303: LC10 = Second(3.5 / 86400)
    Case 1304: LC10 = Hour(-0.25)
    Case 1305: LC10 = Minute(-0.25)
    Case 1306: LC10 = Second(-0.00001)
    Case 1307: LC10 = Hour(-1.5)
    Case 1308: LC10 = Hour(Null)
    Case 1309: LC10 = Hour("13:30")
    Case 1310: LC10 = Hour("1:30 PM")
    Case 1311: LC10 = Minute("13:30:59")
    Case 1312: LC10 = Second("13:30:59.9")
    Case 1313: LC10 = Hour(0.5)
    Case 1314: LC10 = Hour(1 - 1E-12)
    Case 1315: LC10 = Minute(1 - 1E-12)
    Case 1316: LC10 = Second(1 - 1E-12)
    Case 1317: LC10 = Hour(1 - 1E-7)
    Case 1318: LC10 = Second(1 - 1E-7)
    Case 1319: LC10 = Second(1 - 1E-6)
    End Select
    Exit Function
EH:
    LC10 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC11(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1320: LC11 = Hour(0.99999999999)
    Case 1321: LC11 = Hour(43831.999999)
    Case 1322: LC11 = Second(43831.9999999)
    Case 1323: LC11 = Second(43831.99999999)
    Case 1324: LC11 = Second(43831.99999)
    Case 1325: LC11 = Minute(43831.99999)
    Case 1326: LC11 = Minute(43831.999999)
    Case 1327: LC11 = Hour(43831.9999999)
    Case 1328: LC11 = Hour(43831.99999999)
    Case 1329: LC11 = Hour("abc")
    Case 1330: LC11 = Hour(25)
    Case 1331: LC11 = Hour(1E10)
    Case 1332: LC11 = DateValue("2020-01-31")
    Case 1333: LC11 = DateValue("1/2/2020")
    Case 1334: LC11 = DateValue("1/2/2020 13:00")
    Case 1335: LC11 = DateValue("March 5, 2020")
    Case 1336: LC11 = DateValue("5-Mar-2020")
    Case 1337: LC11 = DateValue("5 Mar")
    Case 1338: LC11 = DateValue("Mar 5")
    Case 1339: LC11 = DateValue("13/1/2020")
    Case 1340: LC11 = DateValue("1/13/2020")
    Case 1341: LC11 = DateValue("1/1/30")
    Case 1342: LC11 = DateValue("1/1/29")
    Case 1343: LC11 = DateValue("1/1/100")
    Case 1344: LC11 = DateValue("1/1/99")
    Case 1345: LC11 = DateValue("1/1/0")
    Case 1346: LC11 = DateValue("12:30")
    Case 1347: LC11 = DateValue("")
    Case 1348: LC11 = DateValue(Null)
    Case 1349: LC11 = DateValue(5)
    Case 1350: LC11 = DateValue(#1/2/2020 5:00:00 AM#)
    Case 1351: LC11 = DateValue("2020")
    Case 1352: LC11 = DateValue("Jan 2020")
    Case 1353: LC11 = DateValue("1/2")
    Case 1354: LC11 = DateValue("31/12")
    Case 1355: LC11 = DateValue("2/29/2019")
    Case 1356: LC11 = DateValue("2/29/2020")
    Case 1357: LC11 = DateValue("2/29/1900")
    Case 1358: LC11 = DateValue("12/31/9999")
    Case 1359: LC11 = DateValue("1/1/10000")
    Case 1360: LC11 = DateValue("1/1/0099")
    Case 1361: LC11 = DateValue("Monday 1/2/2020")
    Case 1362: LC11 = DateValue("1/2/2020 abc")
    Case 1363: LC11 = DateValue("2020/1/2")
    Case 1364: LC11 = DateValue("2020/13/2")
    Case 1365: LC11 = DateValue("20200102")
    Case 1366: LC11 = DateValue("1.2.2020")
    Case 1367: LC11 = DateValue("1 2 2020")
    Case 1368: LC11 = DateValue("1-2-2020")
    Case 1369: LC11 = DateValue("1/2/20")
    Case 1370: LC11 = DateValue("Jan 5 2020 5pm")
    Case 1371: LC11 = DateValue("1/2/2020 25:00")
    Case 1372: LC11 = DateValue("&H10")
    Case 1373: LC11 = DateValue("43831")
    Case 1374: LC11 = DateValue("1,2,2020")
    Case 1375: LC11 = DateValue("T")
    Case 1376: LC11 = TimeValue("13:30")
    Case 1377: LC11 = TimeValue("1:30 PM")
    Case 1378: LC11 = TimeValue("1:30 pm")
    Case 1379: LC11 = TimeValue("13:30:59.9")
    Case 1380: LC11 = TimeValue("25:00")
    Case 1381: LC11 = TimeValue("24:00")
    Case 1382: LC11 = TimeValue("12 AM")
    Case 1383: LC11 = TimeValue("12 PM")
    Case 1384: LC11 = TimeValue("13 PM")
    Case 1385: LC11 = TimeValue("0:0")
    Case 1386: LC11 = TimeValue("1/2/2020 13:30")
    Case 1387: LC11 = TimeValue(Null)
    Case 1388: LC11 = TimeValue("")
    Case 1389: LC11 = TimeValue(0.5)
    Case 1390: LC11 = TimeValue(#1/2/2020 5:00:00 AM#)
    Case 1391: LC11 = TimeValue("1:30")
    Case 1392: LC11 = TimeValue("1:30:60")
    Case 1393: LC11 = TimeValue("1:60")
    Case 1394: LC11 = TimeValue("1.30")
    Case 1395: LC11 = TimeValue("5 pm")
    Case 1396: LC11 = TimeValue("17h")
    Case 1397: LC11 = TimeValue("5:30 PM x")
    Case 1398: LC11 = TimeValue("23:59:59")
    Case 1399: LC11 = TimeValue("23:59:59.99")
    Case 1400: LC11 = TimeValue("-1:00")
    Case 1401: LC11 = TimeValue("12:30:45 AM")
    Case 1402: LC11 = MonthName(13)
    Case 1403: LC11 = MonthName(0)
    Case 1404: LC11 = MonthName(1, True)
    Case 1405: LC11 = MonthName(12)
    Case 1406: LC11 = MonthName(1.5)
    Case 1407: LC11 = MonthName(2.5)
    Case 1408: LC11 = MonthName(Null)
    Case 1409: LC11 = MonthName("3")
    Case 1410: LC11 = MonthName(1, 0)
    Case 1411: LC11 = MonthName(1, 5)
    Case 1412: LC11 = MonthName(1, Null)
    Case 1413: LC11 = WeekdayName(1, False, vbMonday)
    Case 1414: LC11 = WeekdayName(8)
    Case 1415: LC11 = WeekdayName(0)
    Case 1416: LC11 = WeekdayName(7, True)
    Case 1417: LC11 = WeekdayName(1, True, 0)
    Case 1418: LC11 = WeekdayName(1, True, 8)
    Case 1419: LC11 = WeekdayName(1, True, 2)
    Case 1420: LC11 = WeekdayName(2, False, 2)
    Case 1421: LC11 = WeekdayName(1.5)
    Case 1422: LC11 = WeekdayName(Null)
    Case 1423: LC11 = WeekdayName(1, Null)
    Case 1424: LC11 = CDate(2958465.99)
    Case 1425: LC11 = CDate(2958466)
    Case 1426: LC11 = CDate(-657434.99)
    Case 1427: LC11 = CDate(-657435)
    Case 1428: LC11 = CDate(0)
    Case 1429: LC11 = CDate(0.5)
    Case 1430: LC11 = CDate(-0.5)
    Case 1431: LC11 = CDate(1.9999999999)
    Case 1432: LC11 = CDate("2020-01-01")
    Case 1433: LC11 = CDate("1/2/2020")
    Case 1434: LC11 = CDate("12:30")
    Case 1435: LC11 = CDate("March 5, 2020")
    Case 1436: LC11 = CDate("abc")
    Case 1437: LC11 = CDate("")
    Case 1438: LC11 = CDate(Null)
    Case 1439: LC11 = CDate(Empty)
    End Select
    Exit Function
EH:
    LC11 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC12(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1440: LC12 = CDate(True)
    Case 1441: LC12 = CDate(fS(0.5))
    Case 1442: LC12 = CDate(fC(1.5))
    Case 1443: LC12 = CDate(CDec(1.5))
    Case 1444: LC12 = CDate("5")
    Case 1445: LC12 = CDate("5.5")
    Case 1446: LC12 = CDate("1/2/2020 13:00")
    Case 1447: LC12 = CDate(#1/2/2020#)
    Case 1448: LC12 = CDate("2020-01-01T10:00:00")
    Case 1449: LC12 = CDate("2020-1-1")
    Case 1450: LC12 = CDate("1 Jan 2020")
    Case 1451: LC12 = CDate("Jan 1")
    Case 1452: LC12 = CDate("1/1")
    Case 1453: LC12 = CDate("12:60")
    Case 1454: LC12 = CDate("12:30:45.5")
    Case 1455: LC12 = CDate("12:30 PM")
    Case 1456: LC12 = CDate("00:00")
    Case 1457: LC12 = CDate("24:00")
    Case 1458: LC12 = CDate("2020-01-01 24:00")
    Case 1459: LC12 = CDate(CVErr(7))
    Case 1460: LC12 = CDate("&H10")
    Case 1461: LC12 = CDate("1e3")
    Case 1462: LC12 = CDate("1,5")
    Case 1463: LC12 = TypeName(CDate("12:30"))
    Case 1464: LC12 = CDbl(CDate("12:30"))
    Case 1465: LC12 = CDbl(CDate("1/2/2020 13:00"))
    Case 1466: LC12 = CDbl(CDate("12:30:45.5"))
    Case 1467: LC12 = CDbl(CDate(0.1 + 0.2))
    Case 1468: LC12 = CDbl(CDate(2.00000000001))
    Case 1469: LC12 = TypeName(#1/1/2020# + 1)
    Case 1470: LC12 = TypeName(#1/1/2020# + fI(1))
    Case 1471: LC12 = TypeName(#1/1/2020# + fD(0.5))
    Case 1472: LC12 = TypeName(#1/1/2020# - #1/1/2019#)
    Case 1473: LC12 = #1/1/2020# - #1/1/2019#
    Case 1474: LC12 = #1/1/2020# + 0.5
    Case 1475: LC12 = TypeName(#1/1/2020# & 1)
    Case 1476: LC12 = #1/1/2020# + "1"
    Case 1477: LC12 = #1/1/2020# + #1/1/2020#
    Case 1478: LC12 = #1/1/2020# * 2
    Case 1479: LC12 = TypeName(#1/1/2020# * 2)
    Case 1480: LC12 = #1/1/2020# \ 2
    Case 1481: LC12 = -#1/1/2020#
    Case 1482: LC12 = TypeName(-#1/1/2020#)
    Case 1483: LC12 = Int(#1/1/2020 5:00:00 AM#)
    Case 1484: LC12 = TypeName(Int(#1/1/2020 5:00:00 AM#))
    Case 1485: LC12 = Fix(#1/1/2020 5:00:00 AM#)
    Case 1486: LC12 = Abs(#1/1/2020#)
    Case 1487: LC12 = TypeName(Abs(#1/1/2020#))
    Case 1488: LC12 = CDbl(#12:00:00 PM#)
    Case 1489: LC12 = CLng(#1/1/2020 12:00:00 PM#)
    Case 1490: LC12 = CLng(#1/1/2020 11:59:00 AM#)
    Case 1491: LC12 = Round(#1/1/2020 12:00:00 PM#)
    Case 1492: LC12 = TypeName(Round(#1/1/2020#))
    Case 1493: LC12 = Sgn(#1/1/2020#)
    Case 1494: LC12 = CStr(#1/1/2020# + 0.5)
    Case 1495: LC12 = Format(#1/1/2020# + 0.5)
    Case 1496: LC12 = #1/1/2020# < #1/2/2020#
    Case 1497: LC12 = #1/1/2020# = 43831
    Case 1498: LC12 = fV(#1/1/2020#) = 43831
    Case 1499: LC12 = fV(#1/1/2020#) = "1/01/2020"
    Case 1500: LC12 = Hex(#1/1/2020#)
    Case 1501: LC12 = Str(#1/1/2020#)
    Case 1502: LC12 = VarType(#1/1/2020#)
    Case 1503: LC12 = IsDate(#1/1/2020#)
    Case 1504: LC12 = IsDate("1/1/2020")
    Case 1505: LC12 = IsDate("abc")
    Case 1506: LC12 = IsDate(Null)
    Case 1507: LC12 = IsDate(43831)
    Case 1508: LC12 = IsDate("43831")
    Case 1509: LC12 = IsDate("12:30")
    Case 1510: LC12 = IsDate("2/30/2020")
    Case 1511: LC12 = IsDate(Empty)
    Case 1512: LC12 = IsDate("")
    Case 1513: LC12 = IsDate("1/1/10000")
    Case 1514: LC12 = IsDate("Jan 1")
    Case 1515: LC12 = IsDate("1")
    Case 1516: LC12 = IsDate("25:00")
    Case 1517: LC12 = IsDate("&H10")
    Case 1518: LC12 = IsDate("1e3")
    Case 1519: LC12 = IsDate(True)
    Case 1520: LC12 = IsDate(CVErr(7))
    Case 1521: LC12 = IsDate("5 pm")
    Case 1522: LC12 = IsDate("2020-01-01")
    Case 1523: LC12 = IsDate("13/1/2020")
    Case 1524: LC12 = CStr(#1/2/2020#)
    Case 1525: LC12 = CStr(#1:05:00 PM#)
    Case 1526: LC12 = CStr(#1/2/2020 1:05:09 PM#)
    Case 1527: LC12 = CStr(CDate(36526.5))
    Case 1528: LC12 = CStr(CDate(0))
    Case 1529: LC12 = CStr(CDate(-1))
    Case 1530: LC12 = CStr(CDate(-1.5))
    Case 1531: LC12 = CStr(CDate(0.99999999))
    Case 1532: LC12 = CStr(CDate(1.5E-5))
    Case 1533: LC12 = CStr(CDate(43831 + 1 / 86400))
    Case 1534: LC12 = CStr(CDate(43831 + 0.4 / 86400))
    Case 1535: LC12 = CStr(CDate(43831 + 0.6 / 86400))
    Case 1536: LC12 = CStr(CDate(43831 + 59.5 / 86400))
    Case 1537: LC12 = CStr(CDate(60))
    Case 1538: LC12 = CStr(CDate(1))
    Case 1539: LC12 = CStr(DateSerial(100, 1, 1))
    Case 1540: LC12 = CStr(DateSerial(9999, 12, 31))
    Case 1541: LC12 = Round(2.5)
    Case 1542: LC12 = Round(0.5)
    Case 1543: LC12 = Round(1.5)
    Case 1544: LC12 = Round(-2.5)
    Case 1545: LC12 = Round(-0.5)
    Case 1546: LC12 = Round(1.005, 2)
    Case 1547: LC12 = Round(2.675, 2)
    Case 1548: LC12 = Round(1.015, 2)
    Case 1549: LC12 = Round(1.025, 2)
    Case 1550: LC12 = Round(0.285, 2)
    Case 1551: LC12 = Round(1234.5678, -2)
    Case 1552: LC12 = Round(1234.5678, 1)
    Case 1553: LC12 = Round(1234.5678, 15)
    Case 1554: LC12 = Round(1234.5678, 20)
    Case 1555: LC12 = Round(0.1 + 0.2, 15)
    Case 1556: LC12 = Round(2.5, 0)
    Case 1557: LC12 = Round(3.5, 0)
    Case 1558: LC12 = Round(fC(2.5))
    Case 1559: LC12 = Round(CDec(2.5))
    End Select
    Exit Function
EH:
    LC12 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC13(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1560: LC13 = Round(CDec(2.675), 2)
    Case 1561: LC13 = Round(CDec(1.005), 2)
    Case 1562: LC13 = Round(fS(2.5))
    Case 1563: LC13 = Round(fS(0.285), 2)
    Case 1564: LC13 = Round(Null)
    Case 1565: LC13 = Round("2.5")
    Case 1566: LC13 = Round("abc")
    Case 1567: LC13 = Round(Empty)
    Case 1568: LC13 = Round(True)
    Case 1569: LC13 = TypeName(Round(fI(2)))
    Case 1570: LC13 = TypeName(Round(fS(2.5)))
    Case 1571: LC13 = TypeName(Round(fC(2.5)))
    Case 1572: LC13 = TypeName(Round(CDec(2.5)))
    Case 1573: LC13 = TypeName(Round("2.5"))
    Case 1574: LC13 = TypeName(Round(fL(2), 1))
    Case 1575: LC13 = Round(2.5, 1.5)
    Case 1576: LC13 = Round(0.15, 1)
    Case 1577: LC13 = Round(0.25, 1)
    Case 1578: LC13 = Round(0.35, 1)
    Case 1579: LC13 = Round(-0.15, 1)
    Case 1580: LC13 = Round(5E-324, 324)
    Case 1581: LC13 = Round(1E+300, -299)
    Case 1582: LC13 = Round(1E+300, 5)
    Case 1583: LC13 = Round(123456789.5)
    Case 1584: LC13 = Round(4503599627370496.5)
    Case 1585: LC13 = Round(2147483647.5)
    Case 1586: LC13 = Round(0.5000000000000001)
    Case 1587: LC13 = Round(0.49999999999999994)
    Case 1588: LC13 = Round(1.45, 1)
    Case 1589: LC13 = Round(8.345, 2)
    Case 1590: LC13 = Round(1E-10, 5)
    Case 1591: LC13 = Round(-1.5)
    Case 1592: LC13 = Round(2.5, -1)
    Case 1593: LC13 = Round(25, -1)
    Case 1594: LC13 = Round(35, -1)
    Case 1595: LC13 = Round(fL(25), -1)
    Case 1596: LC13 = Round(-25, -1)
    Case 1597: LC13 = Round(15, -1)
    Case 1598: LC13 = Round(1250, -2)
    Case 1599: LC13 = Round(1350, -2)
    Case 1600: LC13 = Round(0, 5)
    Case 1601: LC13 = Round(0.5, -1)
    Case 1602: LC13 = Sqr(2)
    Case 1603: LC13 = Sqr(0)
    Case 1604: LC13 = Sqr(-0#)
    Case 1605: LC13 = Sqr(-1)
    Case 1606: LC13 = Sqr(Null)
    Case 1607: LC13 = Sqr("4")
    Case 1608: LC13 = Sqr(Empty)
    Case 1609: LC13 = Sqr(CDec(2))
    Case 1610: LC13 = Sqr(fC(2))
    Case 1611: LC13 = Sqr(1E+308)
    Case 1612: LC13 = Sqr(5E-324)
    Case 1613: LC13 = Log(0)
    Case 1614: LC13 = Log(-1)
    Case 1615: LC13 = Log(1)
    Case 1616: LC13 = Log(10)
    Case 1617: LC13 = Log(5E-324)
    Case 1618: LC13 = Log(Null)
    Case 1619: LC13 = Log("2")
    Case 1620: LC13 = Log(2.718281828459045)
    Case 1621: LC13 = Exp(1)
    Case 1622: LC13 = Exp(709.78)
    Case 1623: LC13 = Exp(709.79)
    Case 1624: LC13 = Exp(-745)
    Case 1625: LC13 = Exp(-746)
    Case 1626: LC13 = Exp(0)
    Case 1627: LC13 = Exp(Null)
    Case 1628: LC13 = Exp(-1000)
    Case 1629: LC13 = Exp(1E-300)
    Case 1630: LC13 = Atn(1) * 4
    Case 1631: LC13 = Atn(1E300)
    Case 1632: LC13 = Atn(-1E300)
    Case 1633: LC13 = Atn(0)
    Case 1634: LC13 = Atn(Null)
    Case 1635: LC13 = Sin(1E22)
    Case 1636: LC13 = Sin(1E300)
    Case 1637: LC13 = Sin(3.141592653589793)
    Case 1638: LC13 = Cos(1.5707963267948966)
    Case 1639: LC13 = Tan(1.5707963267948966)
    Case 1640: LC13 = Tan(0)
    Case 1641: LC13 = Tan(1E22)
    Case 1642: LC13 = Cos(1E22)
    Case 1643: LC13 = Sin(0)
    Case 1644: LC13 = Sin(-0#)
    Case 1645: LC13 = Sin(Null)
    Case 1646: LC13 = Sin("0.5")
    Case 1647: LC13 = Sin(5E-324)
    Case 1648: LC13 = Cos(0)
    Case 1649: LC13 = Sin(1E-200)
    Case 1650: LC13 = Tan(1E-200)
    Case 1651: LC13 = Atn(5E-324)
    Case 1652: LC13 = Sin(fS(1))
    Case 1653: LC13 = TypeName(Sin(fS(1)))
    Case 1654: LC13 = TypeName(Sin(fI(1)))
    Case 1655: LC13 = TypeName(Sqr(fI(4)))
    Case 1656: LC13 = TypeName(Exp(fC(1)))
    Case 1657: LC13 = TypeName(Atn(Null))
    Case 1658: LC13 = TypeName(Sin(Empty))
    Case 1659: LC13 = TypeName(Log(CDec(2)))
    Case 1660: LC13 = Log(10) / Log(2)
    Case 1661: LC13 = fD(2) ^ 0.5 = Sqr(2)
    Case 1662: LC13 = 10 ^ 2
    Case 1663: LC13 = TypeName(10 ^ 2)
    Case 1664: LC13 = Int(2.9999999999999996)
    Case 1665: LC13 = Int(-2.0000000000000004)
    Case 1666: LC13 = Fix(-0.9999999999999999)
    Case 1667: LC13 = Int(1E300)
    Case 1668: LC13 = Int(-1E300)
    Case 1669: LC13 = Int(4503599627370497.5)
    Case 1670: LC13 = Int(CDec(1.99999999999999999999))
    Case 1671: LC13 = Fix(CDec(-1.99999999999999999999))
    Case 1672: LC13 = Int(fS(-0.1))
    Case 1673: LC13 = TypeName(Int(fS(-0.1)))
    Case 1674: LC13 = Int(fL(5))
    Case 1675: LC13 = Int(fV(-5.5))
    Case 1676: LC13 = TypeName(Int(fV(-5.5)))
    Case 1677: LC13 = TypeName(Fix(fV(CDec(-5.5))))
    Case 1678: LC13 = TypeName(Int(CVar(fS(2.5))))
    Case 1679: LC13 = Rnd(-1)
    End Select
    Exit Function
EH:
    LC13 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC14(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1680: LC14 = Rnd(0)
    Case 1681: LC14 = Rnd(1)
    Case 1682: LC14 = Rnd(2)
    Case 1683: LC14 = Rnd(-2)
    Case 1684: LC14 = Rnd(-0.5)
    Case 1685: LC14 = Rnd(Null)
    Case 1686: LC14 = Rnd("a")
    Case 1687: LC14 = TypeName(Rnd())
    Case 1688: LC14 = TypeName(Rnd(-1))
    Case 1689: LC14 = Rnd(-1E10)
    Case 1690: LC14 = Rnd(-1E-10)
    Case 1691: LC14 = Rnd(-123456789)
    Case 1692: LC14 = Rnd(-3.5)
    Case 1693: LC14 = Rnd(-16777216)
    Case 1694: LC14 = Rnd(-16777217)
    Case 1695: LC14 = Rnd(-1E30)
    Case 1696: LC14 = Error(5)
    Case 1697: LC14 = Error$(6)
    Case 1698: LC14 = Error(0)
    Case 1699: LC14 = Error(-1)
    Case 1700: LC14 = Error(65536)
    Case 1701: LC14 = Error(65535)
    Case 1702: LC14 = Error(1000)
    Case 1703: LC14 = Error(32767)
    Case 1704: LC14 = Error(513)
    Case 1705: LC14 = Error(vbObjectError + 1)
    Case 1706: LC14 = Error(Null)
    Case 1707: LC14 = Error(5.5)
    Case 1708: LC14 = Error(6.5)
    Case 1709: LC14 = Error()
    Case 1710: LC14 = TypeName(Error(5))
    Case 1711: LC14 = Error(2147483647)
    Case 1712: LC14 = Error(2007)
    Case 1713: LC14 = Error("5")
    Case 1714: LC14 = Error(7 + 0)
    Case 1715: LC14 = AscB("A")
    Case 1716: LC14 = AscB("" & ChrW(233) & "")
    Case 1717: LC14 = AscB(ChrW(&H4E2D))
    Case 1718: LC14 = AscB(ChrW(&H1234))
    Case 1719: LC14 = AscB("")
    Case 1720: LC14 = AscB(Null)
    Case 1721: LC14 = AscB("abc")
    Case 1722: LC14 = AscB(65)
    Case 1723: LC14 = TypeName(AscB("A"))
    Case 1724: LC14 = LenB(ChrB(65))
    Case 1725: LC14 = AscB(ChrB(200))
    Case 1726: LC14 = TypeName(ChrB(65))
    Case 1727: LC14 = ChrB(256)
    Case 1728: LC14 = ChrB(-1)
    Case 1729: LC14 = ChrB(0) = ChrB(0)
    Case 1730: LC14 = Len(ChrB(65))
    Case 1731: LC14 = LenB(ChrB(65) & ChrB(66))
    Case 1732: LC14 = LenB(ChrB(65) & ChrB(0))
    Case 1733: LC14 = ChrB(65) & ChrB(0) = "A"
    Case 1734: LC14 = ChrB(65) & ChrB(1) = ChrW(&H141)
    Case 1735: LC14 = AscB(ChrB(65) & ChrB(66))
    Case 1736: LC14 = AscW(ChrB(65) & ChrB(66))
    Case 1737: LC14 = AscB(ChrB(1) & ChrB(2) & ChrB(3))
    Case 1738: LC14 = LenB(ChrB(1) & ChrB(2) & ChrB(3))
    Case 1739: LC14 = Len(ChrB(1) & ChrB(2) & ChrB(3))
    Case 1740: LC14 = LenB(MidB("abc", 1, 2))
    Case 1741: LC14 = LenB(MidB("abc", 2))
    Case 1742: LC14 = LenB(MidB("abc", 2, 2))
    Case 1743: LC14 = AscB(MidB("abc", 3, 1))
    Case 1744: LC14 = AscB(MidB("abc", 2, 1))
    Case 1745: LC14 = LenB(MidB("abc", 7))
    Case 1746: LC14 = MidB("abc", 3, 2) = "b"
    Case 1747: LC14 = MidB("abc", 1, 2) = "a"
    Case 1748: LC14 = MidB("abc", 0)
    Case 1749: LC14 = MidB("abc", 1, -1)
    Case 1750: LC14 = TypeName(MidB("abc", 1, 2))
    Case 1751: LC14 = MidB(Null, 1)
    Case 1752: LC14 = LenB(MidB("abc", 3))
    Case 1753: LC14 = LenB(LeftB("abc", 3))
    Case 1754: LC14 = AscB(LeftB("abc", 1))
    Case 1755: LC14 = LenB(RightB("abc", 3))
    Case 1756: LC14 = AscB(RightB("abc", 1))
    Case 1757: LC14 = LeftB("abc", 2) = "a"
    Case 1758: LC14 = RightB("abc", 2) = "c"
    Case 1759: LC14 = LeftB("abc", -1)
    Case 1760: LC14 = LenB(LeftB("abc", 99))
    Case 1761: LC14 = InStrB("abc", "b")
    Case 1762: LC14 = InStrB("abc", ChrB(98))
    Case 1763: LC14 = InStrB("abc", ChrB(98) & ChrB(0))
    Case 1764: LC14 = InStrB(2, "abc", "c")
    Case 1765: LC14 = InStrB(3, "abc", "c")
    Case 1766: LC14 = InStrB("abc", "")
    Case 1767: LC14 = InStrB("", "a")
    Case 1768: LC14 = InStrB("abc", "d")
    Case 1769: LC14 = InStrB(Null, "a")
    Case 1770: LC14 = InStrB("aBc", "b", 1)
    Case 1771: LC14 = InStrB(1, "abc", "bc", vbBinaryCompare)
    Case 1772: LC14 = InStrB("a" & ChrW(&H162), ChrB(&H62))
    Case 1773: LC14 = InStrB("abc", ChrB(0))
    Case 1774: LC14 = InStrB(1, "abc", ChrB(0) & ChrB(99))
    Case 1775: LC14 = LenB(StrConv("abc", 128))
    Case 1776: LC14 = AscB(StrConv("abc", 128))
    Case 1777: LC14 = LenB(StrConv("" & ChrW(233) & "", 128))
    Case 1778: LC14 = LenB(StrConv(ChrW(&H4E2D), 128))
    Case 1779: LC14 = LenB(StrConv("", 128))
    Case 1780: LC14 = Len(StrConv("abc", 128))
    Case 1781: LC14 = StrConv("abc", 128) = "abc"
    Case 1782: LC14 = StrConv(StrConv("abc", 128), 64)
    Case 1783: LC14 = StrConv(StrConv("" & ChrW(233) & "", 128), 64)
    Case 1784: LC14 = LenB(StrConv(ChrB(65) & ChrB(66), 64))
    Case 1785: LC14 = StrConv(ChrB(65) & ChrB(66), 64) = "AB"
    Case 1786: LC14 = StrConv(ChrB(65), 64)
    Case 1787: LC14 = LenB(StrConv("abc", 64))
    Case 1788: LC14 = StrConv("abc", 64)
    Case 1789: LC14 = AscB(StrConv(ChrW(&H4E2D), 128))
    Case 1790: LC14 = StrConv(Null, 128)
    Case 1791: LC14 = StrConv(StrConv("a", 128), 128)
    Case 1792: LC14 = TypeName(StrConv("a", 128))
    Case 1793: LC14 = StrConv("abc", 128 + 1)
    Case 1794: LC14 = StrConv("abc", 128 + 64)
    Case 1795: LC14 = ChrB(65) & "a" = "A" & ChrB(97)
    Case 1796: LC14 = LenB(ChrB(65) & "a")
    Case 1797: LC14 = UCase(ChrB(97)) = ChrB(65)
    Case 1798: LC14 = LenB(UCase(ChrB(97)))
    Case 1799: LC14 = LenB(Left(ChrB(65) & ChrB(66) & ChrB(67), 1))
    End Select
    Exit Function
EH:
    LC14 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC15(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1800: LC15 = LenB(Trim(ChrB(32) & ChrB(65) & ChrB(32)))
    Case 1801: LC15 = LenB(Replace("abc", "b", ChrB(66)))
    Case 1802: LC15 = LenB(CStr(ChrB(65)))
    Case 1803: LC15 = LenB(StrReverse(ChrB(1) & ChrB(2) & ChrB(3) & ChrB(4)))
    Case 1804: LC15 = LenB(String(3, ChrB(65)))
    Case 1805: LC15 = Asc(ChrB(65))
    Case 1806: LC15 = AscW(ChrB(65))
    Case 1807: LC15 = Asc(ChrB(65) & ChrB(66))
    Case 1808: LC15 = CStr(CDate(2958465.99999999))
    Case 1809: LC15 = CStr(CDate(2958465.9999))
    Case 1810: LC15 = CStr(CDate(2958465.5))
    Case 1811: LC15 = CStr(CDate(2958465))
    Case 1812: LC15 = CStr(CDate(2958465.999999))
    Case 1813: LC15 = CStr(CDate(2958465.99999))
    Case 1814: LC15 = CStr(CDate(2958465.99998))
    Case 1815: LC15 = Format(CDate(2958465.99999999), "yyyy-mm-dd hh:nn:ss")
    Case 1816: LC15 = Second(CDate(2958465.99999999))
    Case 1817: LC15 = Hour(CDate(2958465.99999999))
    Case 1818: LC15 = CDate(2958465.99999999) = CDate(2958465)
    Case 1819: LC15 = CDbl(CDate(2958465.99999999))
    Case 1820: LC15 = Day(CDate(2958465.99999999))
    Case 1821: LC15 = Year(CDate(2958465.99999999))
    Case 1822: LC15 = DateAdd("s", 1, CDate(2958465.9999))
    Case 1823: LC15 = CStr(DateAdd("s", -1, CDate(2958466 - 1E-10)))
    Case 1824: LC15 = CStr(CDate(2958465.99999999) - 1E-5)
    Case 1825: LC15 = "" & ChrW(223) & "" = "ss"
    Case 1826: LC15 = fV("" & ChrW(223) & "") = fV("ss")
    Case 1827: LC15 = StrComp("" & ChrW(223) & "", "ss")
    Case 1828: LC15 = "" & ChrW(223) & "" < "ss"
    Case 1829: LC15 = "ss" < "" & ChrW(223) & ""
    Case 1830: LC15 = InStr("" & ChrW(223) & "", "ss")
    Case 1831: LC15 = InStr("ss", "" & ChrW(223) & "")
    Case 1832: LC15 = "" & ChrW(223) & "" Like "ss"
    Case 1833: LC15 = "" & ChrW(223) & "" = "SS"
    Case 1834: LC15 = StrComp("" & ChrW(223) & "", "SS", 1)
    Case 1835: LC15 = "" & ChrW(223) & "" = "s"
    Case 1836: LC15 = "" & ChrW(230) & "" = "ae"
    Case 1837: LC15 = "" & ChrW(198) & "" = "ae"
    Case 1838: LC15 = "" & ChrW(339) & "" = "oe"
    Case 1839: LC15 = StrComp("" & ChrW(230) & "", "ae", 1)
    Case 1840: LC15 = StrComp("" & ChrW(223) & "", "ss", 0)
    Case 1841: LC15 = InStr(1, "x" & ChrW(223) & "y", "ss", 1)
    Case 1842: LC15 = Replace("" & ChrW(223) & "", "ss", "x", 1, -1, 1)
    Case 1843: LC15 = UCase("" & ChrW(223) & "")
    Case 1844: LC15 = LCase("" & ChrW(7838) & "")
    Case 1845: LC15 = StrConv("" & ChrW(223) & "", 1)
    Case 1846: LC15 = "" & ChrW(223) & "" = "" & ChrW(223) & ""
    Case 1847: LC15 = "" & ChrW(223) & "" < "t"
    Case 1848: LC15 = "" & ChrW(223) & "" > "s"
    Case 1849: LC15 = StrComp("" & ChrW(223) & "a", "ssa", 1)
    Case 1850: LC15 = StrComp("ssa", "" & ChrW(223) & "b", 1)
    Case 1851: LC15 = StrComp("" & ChrW(223) & "", "" & ChrW(223) & "", 1)
    Case 1852: LC15 = StrComp("" & ChrW(452) & "", "" & ChrW(454) & "", 1)
    Case 1853: LC15 = StrComp("" & ChrW(454) & "", "d" & ChrW(382) & "", 1)
    Case 1854: LC15 = LP1854()
    Case 1855: LC15 = LP1855()
    Case 1856: LC15 = LP1856()
    Case 1857: LC15 = LP1857()
    Case 1858: LC15 = LP1858()
    Case 1859: LC15 = LP1859()
    Case 1860: LC15 = LP1860()
    Case 1861: LC15 = LP1861()
    Case 1862: LC15 = LP1862()
    Case 1863: LC15 = LP1863()
    Case 1864: LC15 = LP1864()
    Case 1865: LC15 = LP1865()
    Case 1866: LC15 = LP1866()
    Case 1867: LC15 = LP1867()
    Case 1868: LC15 = LP1868()
    Case 1869: LC15 = LP1869()
    Case 1870: LC15 = LP1870()
    Case 1871: LC15 = LP1871()
    Case 1872: LC15 = LP1872()
    Case 1873: LC15 = LP1873()
    Case 1874: LC15 = LP1874()
    Case 1875: LC15 = LP1875()
    Case 1876: LC15 = LP1876()
    Case 1877: LC15 = LP1877()
    Case 1878: LC15 = LP1878()
    Case 1879: LC15 = LP1879()
    Case 1880: LC15 = LP1880()
    Case 1881: LC15 = LP1881()
    Case 1882: LC15 = LP1882()
    Case 1883: LC15 = LP1883()
    Case 1884: LC15 = LP1884()
    Case 1885: LC15 = LP1885()
    Case 1886: LC15 = LP1886()
    Case 1887: LC15 = LP1887()
    Case 1888: LC15 = LP1888()
    Case 1889: LC15 = LP1889()
    Case 1890: LC15 = LP1890()
    Case 1891: LC15 = LP1891()
    Case 1892: LC15 = LP1892()
    Case 1893: LC15 = LP1893()
    Case 1894: LC15 = LP1894()
    Case 1895: LC15 = LP1895()
    Case 1896: LC15 = LP1896()
    Case 1897: LC15 = LP1897()
    Case 1898: LC15 = LP1898()
    Case 1899: LC15 = LP1899()
    Case 1900: LC15 = LP1900()
    Case 1901: LC15 = LP1901()
    Case 1902: LC15 = LP1902()
    Case 1903: LC15 = LP1903()
    Case 1904: LC15 = LP1904()
    Case 1905: LC15 = LP1905()
    Case 1906: LC15 = LP1906()
    Case 1907: LC15 = LP1907()
    Case 1908: LC15 = LP1908()
    Case 1909: LC15 = LP1909()
    Case 1910: LC15 = LP1910()
    Case 1911: LC15 = LP1911()
    Case 1912: LC15 = LP1912()
    Case 1913: LC15 = LP1913()
    Case 1914: LC15 = LP1914()
    Case 1915: LC15 = LP1915()
    Case 1916: LC15 = LP1916()
    Case 1917: LC15 = LP1917()
    Case 1918: LC15 = LP1918()
    Case 1919: LC15 = LP1919()
    End Select
    Exit Function
EH:
    LC15 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LC16(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1920: LC16 = LP1920()
    Case 1921: LC16 = LP1921()
    Case 1922: LC16 = LP1922()
    Case 1923: LC16 = LP1923()
    Case 1924: LC16 = LP1924()
    Case 1925: LC16 = LP1925()
    Case 1926: LC16 = LP1926()
    Case 1927: LC16 = LP1927()
    Case 1928: LC16 = LP1928()
    Case 1929: LC16 = LP1929()
    Case 1930: LC16 = LP1930()
    Case 1931: LC16 = LP1931()
    Case 1932: LC16 = LP1932()
    Case 1933: LC16 = LP1933()
    Case 1934: LC16 = LP1934()
    Case 1935: LC16 = LP1935()
    Case 1936: LC16 = LP1936()
    Case 1937: LC16 = LP1937()
    End Select
    Exit Function
EH:
    LC16 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1854() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String
    For i = 0 To 700
        s = s & i & "=" & Error(i) & vbLf
    Next i
    R = s
    LP1854 = R
    Exit Function
EH:
    LP1854 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1855() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String
    For i = 700 To 2100
        s = s & i & "=" & Error(i) & vbLf
    Next i
    R = s
    LP1855 = R
    Exit Function
EH:
    LP1855 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1856() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String
    For i = 2100 To 32767 Step 7
        s = s & i & "=" & Error(i) & vbLf
    Next i
    R = s
    LP1856 = R
    Exit Function
EH:
    LP1856 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1857() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Long, s As String, v As Variant
    For Each v In Array(0, 1, 5, 6, 9, 11, 13, 28, 35, 51, 91, 380, 438, 513, 1000, 1004, 65535, 65536, 65537, 70000, -1, -5, 2147483647, 3.5, 4.5, "7", "x", Null, Empty, True)
        On Error Resume Next
        Err.Clear
        Err.Raise v
        s = s & "[" & v & "] " & Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.LastDllError & vbLf
        On Error GoTo 0
    Next v
    R = s
    LP1857 = R
    Exit Function
EH:
    LP1857 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1858() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 1000, "src", "desc", "help.chm", 42
    R = Err.Number & "|" & Err.Source & "|" & Err.Description & "|" & Err.HelpFile & "|" & Err.HelpContext
    Err.Raise 5, , "only desc"
    R = R & "#" & Err.Number & "|" & Err.Source & "|" & Err.Description
    Err.Raise 11, "s2"
    R = R & "#" & Err.Number & "|" & Err.Source & "|" & Err.Description
    Err.Raise 1000, , , "h"
    R = R & "#" & Err.Number & "|" & Err.Source & "|" & Err.Description & "|" & Err.HelpFile
    Err.Raise 5, Null, Null
    R = R & "#" & Err.Number & "|" & Err.Source & "|" & Err.Description
    Err.Raise 5, "", ""
    R = R & "#" & Err.Number & "|" & Err.Source & "|" & Err.Description
    Err.Raise 1000, "", ""
    R = R & "#" & Err.Number & "|" & Err.Source & "|" & Err.Description
    LP1858 = R
    Exit Function
EH:
    LP1858 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1859() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim x As Long
    x = 1 / Z()
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Description = "custom"
    R = R & "#" & Err.Number & "|" & Err.Description
    Err.Number = 7
    R = R & "#" & Err.Number & "|" & Err.Description
    Err.Clear
    Err.Number = 7
    R = R & "#" & Err.Number & "|" & Err.Description
    Err.Source = "mysrc"
    R = R & "#" & Err.Source
    Err.Clear
    R = R & "#" & Err.Source & "|" & Err.Number
    Dim o As Object
    o.Foo
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    Dim a(1 To 3) As Long
    a(4) = 1
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    x = CLng("abc")
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    x = CLng(1E+20)
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    Err.Raise 5
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    Err.Raise 1000
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    Err.Raise 1000, "abc"
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    Err.Raise 513, "abc"
    R = R & "#" & Err.Number & "|" & Err.Description & "|" & Err.Source
    Err.Clear
    LP1859 = R
    Exit Function
EH:
    LP1859 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1860() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim i As Integer, l As Long, d As Double, sg As Single, c As Currency, b As Byte, bo As Boolean, dt As Date, st As String, v As Variant, dc As Variant, o As Object, a(1 To 3) As Long, p As TPt, f As String * 5
    i = 1: l = 1: d = 1: sg = 1: c = 1: b = 1: bo = True: dt = 1: st = "abc": dc = CDec(1)
    R = Len(i) & "," & Len(l) & "," & Len(d) & "," & Len(sg) & "," & Len(c) & "," & Len(b) & "," & Len(bo) & "," & Len(dt) & "," & Len(st) & "," & Len(v) & "," & Len(dc) & "," & Len(p) & "," & Len(f)
    R = R & "|" & LenB(i) & "," & LenB(l) & "," & LenB(d) & "," & LenB(sg) & "," & LenB(c) & "," & LenB(b) & "," & LenB(bo) & "," & LenB(dt) & "," & LenB(st) & "," & LenB(v) & "," & LenB(dc) & "," & LenB(p) & "," & LenB(f)
    v = Null
    R = R & "|" & Len(v)
    v = Empty
    R = R & "|" & Len(v) & LenB(v)
    v = 5
    R = R & "|" & Len(v) & LenB(v)
    v = 5.5
    R = R & "|" & Len(v) & LenB(v)

    LP1860 = R
    Exit Function
EH:
    LP1860 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1861() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long, s As String
    On Error Resume Next
    x = 1 / Z()
    s = "a" & Err.Number
    x = 5
    s = s & "b" & Err.Number & x
    Dim y As Long
    y = CLng("zz")
    s = s & "c" & Err.Number
    R = s
    LP1861 = R
    Exit Function
EH:
    LP1861 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1862() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error Resume Next
    If 1 / Z() > 0 Then s = s & "T" Else s = s & "F"
    s = s & Err.Number
    Err.Clear
    For i = 1 To 1 / Z()
        s = s & "L"
    Next i
    s = s & Err.Number & i
    Err.Clear
    Select Case 1 / Z()
        Case 0: s = s & "c0"
        Case Else: s = s & "ce"
    End Select
    s = s & Err.Number
    Err.Clear
    Do While 1 / Z() > 0
        s = s & "D"
        Exit Do
    Loop
    s = s & Err.Number
    R = s
    LP1862 = R
    Exit Function
EH:
    LP1862 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1863() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error GoTo H
    s = "a"
    Err.Raise 5
    s = s & "b"
    GoTo Done
H:
    s = s & "h" & Err.Number
    Resume Next
Done:
    R = s
    LP1863 = R
    Exit Function
EH:
    LP1863 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1864() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long
    On Error GoTo H
    n = n + 1
    s = s & "t" & n
    If n < 3 Then Err.Raise 5
    R = s
    GoTo Fin
H:
    n = n + 1
    s = s & "h"
    If n > 5 Then GoTo Fin
    Resume
Fin:
    LP1864 = R
    Exit Function
EH:
    LP1864 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1865() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error GoTo H
    Err.Raise 5
    s = s & "skipped"
L1:
    s = s & "L1"
    GoTo Fin
H:
    s = s & "h" & Erl
    Resume L1
Fin:
    R = s
    LP1865 = R
    Exit Function
EH:
    LP1865 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1866() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "S"
    On Error GoTo H1
    Err.Raise 5
    GoTo Fin
H1:
    s = s & "1:" & Err.Number
    On Error GoTo H2
    Err.Raise 6
    GoTo Fin
H2:
    s = s & "2:" & Err.Number
Fin:
    R = s
    LP1866 = R
    Exit Function
EH:
    LP1866 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1867() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error GoTo H1
    Err.Raise 5
    GoTo Fin
H1:
    s = s & "n" & Err.Number
    Err.Raise 7
    s = s & "unreached"
Fin:
    R = s
    LP1867 = R
    Exit Function
EH:
    LP1867 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1868() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "w"
    R = Wrapped()
    LP1868 = R
    Exit Function
EH:
    LP1868 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1869() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error GoTo H
    R = GoesUp(s)
    GoTo Fin
H:
    R = s & "|outer" & Err.Number
Fin:
    LP1869 = R
    Exit Function
EH:
    LP1869 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1870() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Dim r1 As Long
    r1 = RaiseIt(5)
    R = r1 & "|" & Err.Number & "|" & Err.Description
    On Error GoTo 0
    LP1870 = R
    Exit Function
EH:
    LP1870 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1871() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = s & Err.Number
    Err.Raise 5
    On Error GoTo 0
    s = s & "|" & Err.Number
    R = s
    LP1871 = R
    Exit Function
EH:
    LP1871 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1872() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error GoTo H
    Err.Raise 5
    s = "no"
    GoTo Fin
H:
    On Error GoTo -1
    s = s & "cleared" & Err.Number
Fin:
    R = s
    LP1872 = R
    Exit Function
EH:
    LP1872 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1873() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Err.Raise 5
    On Error GoTo -1
    s = s & Err.Number
    R = s
    LP1873 = R
    Exit Function
EH:
    LP1873 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1874() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    On Error Resume Next
    For i = 1 To 3
        Err.Raise 100 + i
        s = s & Err.Number & ","
    Next i
    R = s
    LP1874 = R
    Exit Function
EH:
    LP1874 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1875() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    s = s & CStr(1 / Z()) & "|"
    s = s & Err.Number & "|" & Len(s)
    R = s
    LP1875 = R
    Exit Function
EH:
    LP1875 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1876() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    On Error Resume Next
    v = 1 / Z()
    R = TypeName(v) & "|" & Err.Number
    LP1876 = R
    Exit Function
EH:
    LP1876 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1877() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim v As Variant
    v = "keep"
    On Error Resume Next
    v = CLng("abc")
    R = TypeName(v) & "|" & CStr(v) & "|" & Err.Number
    LP1877 = R
    Exit Function
EH:
    LP1877 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1878() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2) As Long, s As String
    On Error Resume Next
    a(3) = 1
    s = Err.Number & Err.Description
    a(Z()) = 1
    s = s & "#" & Err.Number
    Dim b() As Long
    s = s & "#" & UBound(b) & Err.Number & Err.Description
    ReDim b(1 To 2)
    s = s & "#" & b(0) & Err.Number
    R = s
    LP1878 = R
    Exit Function
EH:
    LP1878 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1879() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Dim o As Object
    s = o.Name
    s = s & "|" & Err.Number & Err.Description
    Dim v As Variant
    v = Null
    s = s & "|" & v.Foo & Err.Number & Err.Description
    v = 5
    s = s & "|" & v.Foo & Err.Number & Err.Description
    v = Empty
    s = s & "|" & v.Foo & Err.Number & Err.Description
    Set o = Nothing
    s = s & "|" & o.Foo & Err.Number & Err.Description
    R = s
    LP1879 = R
    Exit Function
EH:
    LP1879 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1880() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    On Error Resume Next
    v = Null
    s = s & "a" & CStr(v) & Err.Number & Err.Description
    Err.Clear
    s = s & "|b" & (v & "x")
    Err.Clear
    s = s & "|c" & Left(v, 1) & Err.Number
    Err.Clear
    s = s & "|d" & Len(v) & Err.Number
    Err.Clear
    Dim t As String
    t = v
    s = s & "|e" & Err.Number & Err.Description
    Err.Clear
    Dim n As Long
    n = v
    s = s & "|f" & Err.Number & Err.Description
    Err.Clear
    Dim d As Date
    d = v
    s = s & "|g" & Err.Number & Err.Description
    Err.Clear
    Dim b As Boolean
    b = v
    s = s & "|h" & Err.Number
    Err.Clear
    Dim o As Object
    Set o = v
    s = s & "|i" & Err.Number & Err.Description
    R = s
    LP1880 = R
    Exit Function
EH:
    LP1880 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1881() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Long, v As Variant, d As Double, i As Integer, b As Byte, c As Currency, sg As Single, dt As Date, bo As Boolean, st As String
    On Error Resume Next
    v = "abc": n = v: s = s & Err.Number & ";": Err.Clear
    v = "": n = v: s = s & Err.Number & ";": Err.Clear
    v = Empty: n = v: s = s & Err.Number & n & ";": Err.Clear
    v = "1e5": i = v: s = s & Err.Number & ";": Err.Clear
    v = 70000: i = v: s = s & Err.Number & Err.Description & ";": Err.Clear
    v = 300: b = v: s = s & Err.Number & ";": Err.Clear
    v = -1: b = v: s = s & Err.Number & ";": Err.Clear
    v = 1E+40: sg = v: s = s & Err.Number & ";": Err.Clear
    v = 1E+40: c = v: s = s & Err.Number & ";": Err.Clear
    v = "x": dt = v: s = s & Err.Number & ";": Err.Clear
    v = "x": bo = v: s = s & Err.Number & ";": Err.Clear
    v = 5: st = v: s = s & Err.Number & st & ";": Err.Clear
    v = #1/2/2020#: st = v: s = s & Err.Number & st & ";": Err.Clear
    v = CVErr(7): n = v: s = s & Err.Number & Err.Description & ";": Err.Clear
    v = CVErr(7): st = v: s = s & Err.Number & ";": Err.Clear
    v = Array(1): n = v: s = s & Err.Number & Err.Description & ";": Err.Clear
    v = Array(1): st = v: s = s & Err.Number & ";": Err.Clear
    n = 3.5: s = s & n & ";"
    n = 2.5: s = s & n & ";"
    n = -2.5: s = s & n & ";"
    i = 0.5: s = s & i & ";"
    i = 1.5: s = s & i & ";"
    b = 0.5: s = s & b & ";"
    b = 1.5: s = s & b & ";"
    n = "7": s = s & n & ";"
    n = True: s = s & n & ";"
    bo = "1": s = s & bo & ";"
    bo = 5: s = s & bo & ";"
    d = "1.5": s = s & d & ";"
    st = 1.5: s = s & st & ";"
    st = True: s = s & st & ";"
    st = Null: s = s & Err.Number & Err.Description & ";": Err.Clear
    i = Null: s = s & Err.Number & ";": Err.Clear
    R = s
    LP1881 = R
    Exit Function
EH:
    LP1881 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1882() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant, k As Variant
    For Each v In Array(0, 1, 3, 5, 6, 10, 11, -1, 2.5, 5.0000001, "5", "abc", "B", Null, Empty, True, "", #1/1/2020#)
        Select Case v
            Case 1, 3
                k = "a"
            Case 4 To 6
                k = "r"
            Case Is > 10
                k = "g"
            Case Is < 0
                k = "n"
            Case "abc"
                k = "s"
            Case "A" To "C"
                k = "sr"
            Case Else
                k = "e"
        End Select
        s = s & k & ","
    Next v
    R = s
    LP1882 = R
    Exit Function
EH:
    LP1882 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1883() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 0 To 8
        Select Case i
            Case 5 To 1: s = s & "rev"
            Case 2 To 4, 7: s = s & "x"
            Case Is >= 8, Is = 0: s = s & "y"
            Case Else: s = s & "-"
        End Select
    Next i
    R = s
    LP1883 = R
    Exit Function
EH:
    LP1883 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1884() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For Each v In Array(1, "1", 1#, "1.0", "a", Null, True, "True")
        Select Case v
            Case 1: s = s & "n1,"
            Case "1": s = s & "s1,"
            Case True: s = s & "T,"
            Case Else: s = s & "e,"
        End Select
    Next v
    R = s
    LP1884 = R
    Exit Function
EH:
    LP1884 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1885() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = -1 To 2
        Select Case True
            Case i < 0: s = s & "neg"
            Case i = 0: s = s & "zero"
            Case i > 0 And i < 2: s = s & "one"
            Case Else: s = s & "big"
        End Select
    Next i
    R = s
    LP1885 = R
    Exit Function
EH:
    LP1885 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1886() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    Dim d As Double
    d = 0.1 + 0.2
    Select Case d
        Case 0.3: s = "eq"
        Case Is > 0.3: s = "gt"
        Case Else: s = "other"
    End Select
    Select Case "abc"
        Case "ABC": s = s & "ci"
        Case Else: s = s & "cs"
    End Select
    Select Case 3
        Case 1 To 5: s = s & "in"
    End Select
    Select Case 3.5
        Case 3: s = s & "3"
        Case 4: s = s & "4"
        Case Else: s = s & "else"
    End Select
    Select Case CByte(3)
        Case 3.4: s = s & "b3.4"
        Case Else: s = s & "belse"
    End Select
    Select Case Null
        Case Null: s = s & "nn"
        Case Else: s = s & "nelse"
    End Select
    R = s
    LP1886 = R
    Exit Function
EH:
    LP1886 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1887() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    Select Case "a"
        Case 1: s = "one"
        Case Else: s = "else"
    End Select
    R = s
    LP1887 = R
    Exit Function
EH:
    LP1887 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1888() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    Select Case 5
        Case 1 To 10, 20: s = "x"
    End Select
    Select Case Colr.Blue
        Case Red: s = s & "r"
        Case Is >= Green: s = s & "g+"
    End Select
    R = s & EnumName(Red) & EnumName(Green) & EnumName(Blue) & EnumName(7) & Blue & Last & Colr.Last
    LP1888 = R
    Exit Function
EH:
    LP1888 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1889() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 0
        s = s & "x"
    Next i
    s = s & i
    For i = 5 To 1 Step -2
        s = s & i
    Next i
    s = s & "|" & i
    For i = 1 To 3
        i = i + 1
        s = s & i
    Next i
    s = s & "|" & i
    R = s
    LP1889 = R
    Exit Function
EH:
    LP1889 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1890() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, d As Double
    For d = 0 To 1 Step 0.1
        s = s & d & ","
    Next d
    s = s & "|" & d
    R = s
    LP1890 = R
    Exit Function
EH:
    LP1890 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1891() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, d As Single
    For d = 0 To 1 Step 0.1
        s = s & d & ","
    Next d
    s = s & "|" & d
    R = s
    LP1891 = R
    Exit Function
EH:
    LP1891 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1892() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Integer
    For i = 32765 To 32767
        s = s & i & ","
    Next i
    R = s & i
    LP1892 = R
    Exit Function
EH:
    LP1892 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1893() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, b As Byte
    For b = 253 To 255
        s = s & b & ","
        If Len(s) > 20 Then Exit For
    Next b
    R = s & b
    LP1893 = R
    Exit Function
EH:
    LP1893 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1894() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 2147483645 To 2147483647
        s = s & i & ","
        If Len(s) > 40 Then Exit For
    Next i
    R = s & i
    LP1894 = R
    Exit Function
EH:
    LP1894 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1895() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, n As Long
    n = 3
    For i = 1 To n
        n = 10
        s = s & i
    Next i
    R = s & "|" & n
    LP1895 = R
    Exit Function
EH:
    LP1895 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1896() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, st As Long
    st = 1
    For i = 1 To 6 Step st
        st = 2
        s = s & i
    Next i
    R = s
    LP1896 = R
    Exit Function
EH:
    LP1896 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1897() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 3.5
        s = s & i
    Next i
    R = s
    LP1897 = R
    Exit Function
EH:
    LP1897 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1898() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant
    For v = 1 To 3
        s = s & TypeName(v) & v
    Next v
    For v = 1# To 2#
        s = s & TypeName(v)
    Next v
    For v = "1" To "3"
        s = s & TypeName(v)
    Next v
    R = s
    LP1898 = R
    Exit Function
EH:
    LP1898 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1899() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, j As Long
    For i = 1 To 3
        For j = 1 To 3
            If j = 2 Then Exit For
            s = s & i & j & ","
        Next j
    Next i
    R = s
    LP1899 = R
    Exit Function
EH:
    LP1899 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1900() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 5
        If i = 2 Then GoTo Skip
        s = s & i
Skip:
    Next i
    R = s
    LP1900 = R
    Exit Function
EH:
    LP1900 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1901() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    For i = 1 To 3
        s = s & i
    Next
    R = s
    LP1901 = R
    Exit Function
EH:
    LP1901 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1902() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, j As Long
    For i = 1 To 2
        For j = 1 To 2
            s = s & i & j
    Next j, i
    R = s
    LP1902 = R
    Exit Function
EH:
    LP1902 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1903() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, v As Variant, a(1 To 3) As Long, i As Long
    For Each v In a
        v = 7
        s = s & v
    Next v
    s = s & a(1)
    Dim w As Variant
    w = Array(1, 2, 3)
    For Each v In w
        s = s & v
        w(0) = 99
    Next v
    For Each v In Array()
        s = s & "never"
    Next v
    Dim q() As Long
    ReDim q(1 To 2, 1 To 2)
    q(1, 1) = 1: q(2, 1) = 2: q(1, 2) = 3: q(2, 2) = 4
    For Each v In q
        s = s & v
    Next v
    R = s
    LP1903 = R
    Exit Function
EH:
    LP1903 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1904() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long, v As Variant
    For Each v In Array(1, 2, 3)
        If v = 2 Then Exit For
        s = s & v
    Next v
    s = s & "|" & v
    R = s
    LP1904 = R
    Exit Function
EH:
    LP1904 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1905() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, i As Long
    Do While i < 3
        i = i + 1
        s = s & i
    Loop
    Do Until i <= 0
        i = i - 1
        s = s & i
    Loop
    Do
        i = i + 1
        s = s & i
    Loop While i < 2
    Do
        i = i + 1
        s = s & i
    Loop Until True
    While i < 6
        i = i + 1
        s = s & i
    Wend
    Do
        i = i + 1
        If i > 8 Then Exit Do
    Loop
    R = s & "|" & i
    LP1905 = R
    Exit Function
EH:
    LP1905 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1906() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, n As Variant
    n = Null
    Do While n
        s = s & "x"
        Exit Do
    Loop
    s = s & "a"
    If n Then s = s & "T" Else s = s & "F"
    n = "1"
    If n Then s = s & "T" Else s = s & "F"
    n = "x"
    If n Then s = s & "T" Else s = s & "F"
    R = s
    LP1906 = R
    Exit Function
EH:
    LP1906 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1907() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    If 2.5 Then s = s & "a"
    If 0.4 Then s = s & "b"
    If "0" Then s = s & "c"
    If Empty Then s = s & "d" Else s = s & "e"
    If CVar(5) And 2 Then s = s & "f" Else s = s & "g"
    If 5 And 2 Then s = s & "h" Else s = s & "i"
    If True Or 1 / Z() Then s = s & "j"
    If False And 1 / Z() Then s = s & "k" Else s = s & "l"
    R = s
    LP1907 = R
    Exit Function
EH:
    LP1907 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1908() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, x As Long
    x = 5
    If x = 5 Then s = "a": s = s & "b" Else s = "no"
    If x = 4 Then s = "no" Else If x = 5 Then s = s & "c" Else s = s & "d"
    R = s
    LP1908 = R
    Exit Function
EH:
    LP1908 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1909() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Opt1() & "#" & Opt1(1) & "#" & Opt1(, "y") & "#" & Opt1(c:=7) & "#" & Opt1(b:="q", a:=2) & "#" & Opt1(1, "z", Empty) & "#" & Opt1(1, , Null)
    LP1909 = R
    Exit Function
EH:
    LP1909 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1910() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Opt2() & "#" & Opt2(1) & "#" & Opt2(, 4) & "#" & Opt2(Empty) & "#" & Opt2(Null, Null) & "#" & Opt2(b:=Empty)
    LP1910 = R
    Exit Function
EH:
    LP1910 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1911() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Opt3() & "#" & Opt3(1, 2, True, 3, 4)
    LP1911 = R
    Exit Function
EH:
    LP1911 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1912() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = PA() & "#" & PA(1) & "#" & PA(1, "a", Empty, 2.5, #1/1/2020#) & "#" & PA(Array(1, 2)) & "#" & PA(0) & "#" & PA2(1) & "#" & PA2(1, 2, 3) & "#" & PA2(1, Array(1, 2))
    LP1912 = R
    Exit Function
EH:
    LP1912 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1913() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long, y As Long, s As String
    x = 1: y = 1
    ByRefInc x
    ByValInc y
    s = x & y
    ByRefInc (x)
    s = s & x
    ByRefInc x + 0
    s = s & x
    Call ByRefInc(x)
    s = s & x
    Dim v As Variant
    v = "a"
    ByRefV v
    s = s & v
    Dim a(1 To 3) As Long
    ByRefInc a(2)
    ByRefInc a(2)
    s = s & a(2)
    Dim d As Double
    d = 1
    ByRefD d
    s = s & d
    Dim arr() As Long
    ReDim arr(1 To 3)
    ByRefArr arr
    s = s & arr(1)
    Dim p As Long, q As Long
    SetBoth p, p
    s = s & p
    SetBoth q, y
    s = s & q & y
    R = s
    LP1913 = R
    Exit Function
EH:
    LP1913 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1914() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = StaticCtr() & StaticCtr() & StaticCtr() & "|" & StaticFn() & StaticFn() & "|" & DoubleStatic() & DoubleStatic()
    R = s
    LP1914 = R
    Exit Function
EH:
    LP1914 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1915() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Fact(5) & "|" & Fact(20) & "|" & Fact(170) & "|" & Fact(171) & "|" & Fact(0) & "|" & Fact(-3)
    LP1915 = R
    Exit Function
EH:
    LP1915 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1916() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Deep(100) & "|" & Deep(2000) & "|" & Deep(5000)
    LP1916 = R
    Exit Function
EH:
    LP1916 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1917() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    On Error Resume Next
    Dim n As Long
    n = Inf(0)
    R = n & "|" & Err.Number & "|" & Err.Description
    LP1917 = R
    Exit Function
EH:
    LP1917 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1918() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = FnNoAssign() & "|" & TypeName(FnVNoAssign()) & "|" & Len(FnSNoAssign()) & "|" & FnOExit(1) & FnOExit(2)
    LP1918 = R
    Exit Function
EH:
    LP1918 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1919() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant
    a = RetArr()
    R = TypeName(a) & "|" & LBound(a) & UBound(a) & "|" & a(2) & "|" & PGet & "|" & PGet() & "|" & UBound(RetV()) & "|" & TypeName(RetV()(2)) & "|" & RetV()(1) & "|" & UBound(RetArr)
    LP1919 = R
    Exit Function
EH:
    LP1919 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1920() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = K1 & "|" & K2 & "|" & TypeName(K2) & "|" & K3 & "|" & TypeName(K1) & "|" & vbCrLf & "|" & vbTab & "|" & Len(vbNullString) & "|" & vbObjectError & "|" & vbYes & vbNo & vbCancel & "|" & vbBinaryCompare & vbTextCompare & vbDatabaseCompare & "|" & vbSunday & vbUseSystemDayOfWeek & "|" & TypeName(vbNullChar) & Len(vbNullChar) & "|" & vbFirstJan1 & vbFirstFourDays & vbFirstFullWeek & "|" & vbGeneralDate & vbLongDate & vbShortDate & vbLongTime & vbShortTime & "|" & vbUpperCase & vbLowerCase & vbProperCase & vbWide & vbNarrow & vbKatakana & vbHiragana & vbUnicode & vbFromUnicode & "|" & vbEmpty & vbNull & vbInteger & vbLong & vbSingle & vbDouble & vbCurrency & vbDate & vbString & vbObject & vbError & vbBoolean & vbVariant & vbDataObject & vbDecimal & vbByte & vbArray & "|" & vbCr & vbLf & vbFormFeed & vbVerticalTab & vbBack & "|" & vbNewLine
    LP1920 = R
    Exit Function
EH:
    LP1920 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1921() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(5) As Long, b(1 To 3, 0 To 2) As Integer, c() As Double, s As String
    s = LBound(a) & UBound(a) & "|" & LBound(b, 1) & UBound(b, 1) & LBound(b, 2) & UBound(b, 2)
    ReDim c(2 To 4)
    s = s & "|" & LBound(c) & UBound(c) & "|" & c(3)
    ReDim Preserve c(2 To 6)
    s = s & "|" & UBound(c)
    c(6) = 1.5
    ReDim Preserve c(2 To 5)
    s = s & "|" & UBound(c) & c(5)
    ReDim c(1 To 2)
    s = s & "|" & c(1)
    Erase c
    On Error Resume Next
    s = s & "|" & UBound(c)
    s = s & Err.Number
    R = s
    LP1921 = R
    Exit Function
EH:
    LP1921 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1922() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim m() As Long, s As String
    ReDim m(1 To 2, 1 To 3)
    m(2, 3) = 7
    ReDim Preserve m(1 To 2, 1 To 5)
    s = m(2, 3) & UBound(m, 2)
    On Error Resume Next
    ReDim Preserve m(1 To 3, 1 To 5)
    s = s & "|" & Err.Number & Err.Description
    Err.Clear
    ReDim Preserve m(0 To 2, 1 To 5)
    s = s & "|" & Err.Number
    Err.Clear
    ReDim Preserve m(1 To 2, 2 To 5)
    s = s & "|" & Err.Number
    Err.Clear
    s = s & "|" & UBound(m, 3) & Err.Number & Err.Description
    Err.Clear
    s = s & "|" & UBound(m, 0) & Err.Number
    R = s
    LP1922 = R
    Exit Function
EH:
    LP1922 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1923() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a As Variant, s As String
    a = Array(1, 2, 3)
    s = LBound(a) & UBound(a)
    Dim b As Variant
    b = a
    b(0) = 99
    s = s & "|" & a(0) & b(0)
    Dim c() As Long
    c = a
    s = s & "|" & TypeName(c) & c(2)
    ReDim c(1 To 2)
    c(1) = 5
    Dim d() As Long
    d = c
    d(1) = 6
    s = s & "|" & c(1) & d(1) & LBound(d)
    Dim e As Variant
    e = Array(Array(1, 2), Array(3))
    s = s & "|" & e(0)(1) & UBound(e(1)) & TypeName(e(0))
    Dim g(1 To 2) As Variant
    g(1) = Array(1)
    s = s & "|" & TypeName(g(1)) & TypeName(g(2))
    R = s
    LP1923 = R
    Exit Function
EH:
    LP1923 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1924() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a As Variant
    s = IsArray(Array()) & IsArray(1) & UBound(Array()) & LBound(Array())
    a = Array()
    s = s & "|" & UBound(a)
    Dim v As Variant
    s = s & "|" & IsArray(v)
    ReDim v(1 To 2)
    s = s & IsArray(v) & TypeName(v) & VarType(v)
    Dim i() As Integer
    s = s & "|" & TypeName(i) & VarType(i) & IsArray(i)
    ReDim i(1 To 2)
    s = s & TypeName(i) & VarType(i)
    s = s & "|" & TypeName(Array(1)) & VarType(Array(1))
    R = s
    LP1924 = R
    Exit Function
EH:
    LP1924 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1925() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a(1 To 3) As String, b(1 To 3) As Long, c(1 To 2) As Variant, d(1 To 2) As Double, e(1 To 2) As Boolean, f(1 To 2) As Date, g(1 To 2) As Currency, h(1 To 2) As Object
    s = "[" & a(1) & "]" & b(1) & TypeName(c(1)) & d(1) & e(1) & CDbl(f(1)) & g(1) & TypeName(h(1)) & Len(a(2))
    Dim fs As String * 5
    s = s & "|" & Len(fs) & "[" & fs & "]"
    fs = "abcdefgh"
    s = s & fs & Len(fs)
    fs = "ab"
    s = s & "[" & fs & "]" & Len(fs) & Len(Trim(fs))
    Dim fl As String * 3
    s = s & "|" & Asc(fl) & Len(fl)
    R = s
    LP1925 = R
    Exit Function
EH:
    LP1925 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1926() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 3) As Long, s As String, i As Long
    On Error Resume Next
    For i = 0 To 4
        a(i) = i
        s = s & Err.Number & ","
        Err.Clear
    Next i
    R = s & a(1) & a(3)
    LP1926 = R
    Exit Function
EH:
    LP1926 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1927() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    ReDim mArr(1 To 3)
    mArr(2) = 5
    s = UBound(mArr) & mArr(2)
    ReDim Preserve mArr(1 To 5)
    s = s & UBound(mArr) & mArr(2)
    Erase mArr
    On Error Resume Next
    s = s & "|" & UBound(mArr) & Err.Number
    R = s
    LP1927 = R
    Exit Function
EH:
    LP1927 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1928() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, a(1 To 3) As Long, i As Long
    a(1) = 1: a(2) = 2: a(3) = 3
    Erase a
    s = a(1) & a(2) & a(3) & UBound(a)
    Dim v(1 To 2) As Variant
    v(1) = 5
    Erase v
    s = s & TypeName(v(1)) & UBound(v)
    Dim st(1 To 2) As String
    st(1) = "x"
    Erase st
    s = s & "[" & st(1) & "]"
    R = s
    LP1928 = R
    Exit Function
EH:
    LP1928 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1929() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2, 1 To 2) As Long, s As String
    a(1, 1) = 1: a(2, 1) = 2: a(1, 2) = 3: a(2, 2) = 4
    Dim v As Variant
    v = a
    s = TypeName(v) & VarType(v) & UBound(v, 1) & UBound(v, 2) & v(2, 1)
    Dim rr() As Long
    rr = v
    s = s & rr(1, 2)
    R = s
    LP1929 = R
    Exit Function
EH:
    LP1929 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1930() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As TPt, q As TPt, s As String
    s = p.X & "|" & p.Y & "|[" & p.S & "]|" & p.A(1) & LBound(p.A) & UBound(p.A)
    p.X = 5: p.Y = 1.5: p.S = "hi": p.A(2) = 7
    q = p
    q.X = 6
    q.A(2) = 8
    s = s & "|" & p.X & q.X & p.A(2) & q.A(2) & q.S
    Dim b As TBox
    b.P.X = 1: b.Q.X = 2: b.N = "abcdefg"
    s = s & "|" & b.P.X & b.Q.X & b.N & Len(b.N)
    Dim c As TBox
    c = b
    c.P.X = 9
    s = s & "|" & b.P.X & c.P.X
    With b.Q
        .X = 5
        .Y = .X * 2
    End With
    s = s & "|" & b.Q.X & b.Q.Y
    R = s
    LP1930 = R
    Exit Function
EH:
    LP1930 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1931() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim p As TPt, s As String
    p.X = 2147483647
    On Error Resume Next
    p.X = p.X + 1
    s = Err.Number & "|" & p.X
    p.A(1) = 40000
    s = s & "|" & Err.Number
    p.S = 12.5
    s = s & "|" & p.S
    p.Y = "3"
    s = s & "|" & p.Y
    R = s
    LP1931 = R
    Exit Function
EH:
    LP1931 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1932() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double
    d = 1
    On Error Resume Next
    d = d / Z()
    R = d & "|" & Err.Number & "|" & Err.Description
    Err.Clear
    d = 5
    d = d / 0
    R = R & "|" & d & "|" & Err.Number
    LP1932 = R
    Exit Function
EH:
    LP1932 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1933() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim d As Double, n As Long
    d = 7
    On Error Resume Next
    d = fD(5) \ Z()
    n = fL(5) Mod Z()
    R = d & "|" & n & "|" & Err.Number
    Dim v As Variant
    v = 3
    v = v / Z()
    R = R & "|" & v & "|" & TypeName(v)
    Dim i As Long
    i = 9
    i = i / Z()
    R = R & "|" & i
    Dim c As Currency
    c = 4
    c = c / Z()
    R = R & "|" & c
    LP1933 = R
    Exit Function
EH:
    LP1933 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1934() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, k As Variant
    For Each k In Array(1, 5, 6, 7, 9, 11, 13, 53, 57, 91, 92, 94, 321, 380, 438, 1004, 2042)
        On Error Resume Next
        Err.Clear
        Err.Raise k
        s = s & k & ":" & Err.HelpFile & ":" & Err.HelpContext & ":" & Err.Source & ";"
    Next k
    R = s
    LP1934 = R
    Exit Function
EH:
    LP1934 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1935() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, k As Variant
    For Each k In Array(1, 5, 7, 9, 11, 13, 53, 91, 380, 438, 513, 1004, 31999, 65535, 65536, -1, 0)
        On Error Resume Next
        Err.Clear
        Err.Raise k
        s = s & k & "=" & Err.Number & "/" & Err.HelpContext & "/" & Err.Description & ";"
    Next k
    R = s
    LP1935 = R
    Exit Function
EH:
    LP1935 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1936() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim a(1 To 2) As Long
    On Error Resume Next
    a(3) = 1
    R = Err.Number & "|" & Err.HelpFile & "|" & Err.HelpContext
    Err.Clear
    R = R & "|" & Err.HelpFile & "|" & Err.HelpContext
    Dim x As Long
    x = CLng("zz")
    R = R & "|" & Err.HelpFile & "|" & Err.HelpContext & "|" & Err.Source & "|" & Err.LastDllError
    Err.Raise 5, "src", "desc", "my.hlp", 77
    R = R & "|" & Err.HelpFile & "|" & Err.HelpContext
    LP1936 = R
    Exit Function
EH:
    LP1936 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function LP1937() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String, k As Variant
    For Each k In Array(vbObjectError, vbObjectError + 1, vbObjectError + 513, -2147221504, -2147352567, -2147024809, -2146828288, -2146827284)
        On Error Resume Next
        Err.Clear
        Err.Raise k
        s = s & k & "=" & Err.Number & "/" & Err.HelpContext & "/" & Err.Description & ";"
        Err.Clear
        Error k
        s = s & Err.Number & "/" & Err.Description & ";"
    Next k
    R = s
    LP1937 = R
    Exit Function
EH:
    LP1937 = "#ERR " & Err.Number & " " & Err.Description
End Function
