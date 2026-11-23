Attribute VB_Name = "VbFz1Udf"
Option Explicit
Public Function Fz_U0(ByVal a As Long, Optional ByVal b As Long = 2) As Currency
    On Error Resume Next
    Fz_U0 = a = b
End Function
Public Function Fz_U1(a As Currency, Optional b As Currency = 2) As Integer
    Fz_U1 = a Like "*1*"
End Function
Public Function Fz_U2(ByVal a As Variant, Optional ByVal b As Single = 2) As String
    On Error Resume Next
    Fz_U2 = Format(a, "0.00")
End Function
Public Function Fz_U3(ByVal a As Currency, Optional ByVal b As Date = 2) As Long
    On Error Resume Next
    Fz_U3 = a = b
End Function
Public Function Fz_U4(ByVal a As Single, Optional ByVal b As String = 2) As Boolean
    On Error Resume Next
    Fz_U4 = a = b
End Function
Public Function Fz_U5(a As Variant, Optional b As Byte = 2) As Double
    On Error Resume Next
    Fz_U5 = a = b
End Function
Public Function Fz_U6(ByVal a As Double, Optional ByVal b As Single = 2) As Long
    Fz_U6 = a > b
End Function
Public Function Fz_U7(ByVal a As Integer, Optional ByVal b As Date = 2) As Long
    On Error Resume Next
    Fz_U7 = a & b
End Function
Public Function Fz_U8(a As Variant, Optional b As Integer = 2) As String
    Fz_U8 = a + b
End Function
Public Function Fz_U9(a As Single, Optional b As String = 2) As Currency
    Fz_U9 = a * b
End Function
Public Function Fz_U10(ByVal a As Currency, Optional ByVal b As Boolean = 2) As Double
    On Error Resume Next
    Fz_U10 = a Like "*1*"
End Function
Public Function Fz_U11(ByVal a As Boolean, Optional ByVal b As Currency = 2) As Long
    Fz_U11 = IIf(a, b, -b)
End Function
Public Function Fz_U12(ByVal a As Currency, Optional ByVal b As Boolean = 2) As Date
    On Error Resume Next
    Fz_U12 = Val(a)
End Function
Public Function Fz_U13(a As Byte, Optional b As Single = 2) As Date
    Fz_U13 = a Mod b
End Function
Public Function Fz_U14(ByVal a As Date, Optional ByVal b As Currency = 2) As Date
    On Error Resume Next
    Fz_U14 = a \ b
End Function
Public Function Fz_U15(ByVal a As String, Optional ByVal b As Date = 2) As Currency
    On Error Resume Next
    Fz_U15 = Array(a, b)
End Function
Public Function Fz_U16(a As Variant, Optional b As Long = 2) As Long
    On Error Resume Next
    Fz_U16 = UCase(a)
End Function
Public Function Fz_U17(ByVal a As Date, Optional ByVal b As Date = 2) As Long
    Fz_U17 = CLng(a) + CInt(b)
End Function
Public Function Fz_U18(ByVal a As Double, Optional ByVal b As Single = 2) As Single
    On Error Resume Next
    Fz_U18 = a Mod b
End Function
Public Function Fz_U19(ByVal a As Double, Optional ByVal b As Variant ) As String
    On Error Resume Next
    Fz_U19 = UCase(a)
End Function
Public Function Fz_U20(ByVal a As Byte, Optional ByVal b As String = 2) As Variant
    Fz_U20 = a = b
End Function
Public Function Fz_U21(a As Byte, Optional b As String = 2) As Single
    Fz_U21 = a > b
End Function
Public Function Fz_U22(ByVal a As Boolean, Optional ByVal b As Integer = 2) As String
    Fz_U22 = a = b
End Function
Public Function Fz_U23(ByVal a As Single, Optional ByVal b As Byte = 2) As Date
    Fz_U23 = a Like "*1*"
End Function
Public Function Fz_U24(a As Double, Optional b As Boolean = 2) As Double
    Fz_U24 = Array(a, b)
End Function
Public Function Fz_U25(a As Byte, Optional b As Variant ) As Date
    On Error Resume Next
    Fz_U25 = a - b
End Function
Public Function Fz_U26(ByVal a As Date, Optional ByVal b As Boolean = 2) As Variant
    Fz_U26 = CDec(a) + b
End Function
Public Function Fz_U27(a As Currency, Optional b As Single = 2) As Variant
    On Error Resume Next
    Fz_U27 = a / b
End Function
Public Function Fz_U28(ByVal a As Currency, Optional ByVal b As Double = 2) As Long
    Fz_U28 = Array(a, b)
End Function
Public Function Fz_U29(a As Variant, Optional b As Single = 2) As String
    Fz_U29 = a ^ b
End Function
Public Function Fz_U30(a As Variant, Optional b As Integer = 2) As String
    On Error Resume Next
    Fz_U30 = Val(a)
End Function
Public Function Fz_U31(a As String, Optional b As Long = 2) As Currency
    Fz_U31 = a / b
End Function
Public Function Fz_U32(a As Long, Optional b As Single = 2) As Double
    On Error Resume Next
    Fz_U32 = a \ b
End Function
Public Function Fz_U33(ByVal a As Date, Optional ByVal b As Boolean = 2) As Double
    Fz_U33 = IIf(a, b, -b)
End Function
Public Function Fz_U34(a As Double, Optional b As Single = 2) As Single
    Fz_U34 = Val(a)
End Function
Public Function Fz_U35(ByVal a As Boolean, Optional ByVal b As String = 2) As Boolean
    Fz_U35 = CCur(a) * b
End Function
Public Function Fz_U36(ByVal a As Single, Optional ByVal b As Double = 2) As Integer
    Fz_U36 = a / b
End Function
Public Function Fz_U37(ByVal a As String, Optional ByVal b As Double = 2) As Single
    Fz_U37 = a ^ b
End Function
Public Function Fz_U38(a As Integer, Optional b As Boolean = 2) As Integer
    On Error Resume Next
    Fz_U38 = Array(Array(a), b)
End Function
Public Function Fz_U39(ByVal a As Double, Optional ByVal b As Variant ) As String
    Fz_U39 = CSng(a) / 3
End Function
Public Function Fz_U40(ByVal a As Byte, Optional ByVal b As Double = 2) As Date
    Fz_U40 = IIf(a, b, -b)
End Function
Public Function Fz_U41(ByVal a As Boolean, Optional ByVal b As String = 2) As Single
    On Error Resume Next
    Fz_U41 = a = b
End Function
Public Function Fz_U42(ByVal a As Date, Optional ByVal b As Boolean = 2) As Integer
    On Error Resume Next
    Fz_U42 = IIf(a, b, -b)
End Function
Public Function Fz_U43(a As Date, Optional b As Variant ) As Date
    Fz_U43 = a Like "*1*"
End Function
Public Function Fz_U44(ByVal a As String, Optional ByVal b As Double = 2) As Date
    Fz_U44 = Int(a) - Fix(b)
End Function
Public Function Fz_U45(ByVal a As Byte, Optional ByVal b As Variant ) As Date
    Fz_U45 = a & b
End Function
Public Function Fz_U46(ByVal a As Integer, Optional ByVal b As Boolean = 2) As Double
    On Error Resume Next
    Fz_U46 = Array(Array(a), b)
End Function
Public Function Fz_U47(ByVal a As Long, Optional ByVal b As Boolean = 2) As Single
    On Error Resume Next
    Fz_U47 = a * b
End Function
Public Function Fz_U48(ByVal a As Double, Optional ByVal b As Double = 2) As Integer
    Fz_U48 = a = b
End Function
Public Function Fz_U49(ByVal a As Date, Optional ByVal b As Long = 2) As Variant
    On Error Resume Next
    Fz_U49 = Format(a, "0.00")
End Function
Public Function Fz_U50(ByVal a As Byte, Optional ByVal b As String = 2) As Currency
    Fz_U50 = Empty
End Function
Public Function Fz_U51(ByVal a As String, Optional ByVal b As Single = 2) As Long
    On Error Resume Next
    Fz_U51 = a & b
End Function
Public Function Fz_U52(a As Boolean, Optional b As Boolean = 2) As Boolean
    On Error Resume Next
    Fz_U52 = Array(Array(a), b)
End Function
Public Function Fz_U53(ByVal a As Single, Optional ByVal b As Integer = 2) As String
    Fz_U53 = Null
End Function
Public Function Fz_U54(a As String, Optional b As Double = 2) As Double
    Fz_U54 = a & b
End Function
Public Function Fz_U55(a As Variant, Optional b As Double = 2) As Variant
    Fz_U55 = a + b
End Function
Public Function Fz_U56(a As Date, Optional b As Long = 2) As Currency
    Fz_U56 = a Like "*1*"
End Function
Public Function Fz_U57(a As Single, Optional b As Single = 2) As String
    Fz_U57 = Array(a, b)
End Function
Public Function Fz_U58(a As Currency, Optional b As Integer = 2) As Date
    Fz_U58 = a / b
End Function
Public Function Fz_U59(ByVal a As Integer, Optional ByVal b As Long = 2) As String
    On Error Resume Next
    Fz_U59 = CLng(a) + CInt(b)
End Function
Public Function Fz_U60(ByVal a As String, Optional ByVal b As Single = 2) As Date
    Fz_U60 = Array(a, b)
End Function
Public Function Fz_U61(a As Variant, Optional b As Long = 2) As Long
    On Error Resume Next
    Fz_U61 = a * b
End Function
Public Function Fz_U62(ByVal a As Date, Optional ByVal b As String = 2) As Boolean
    Fz_U62 = a Mod b
End Function
Public Function Fz_U63(a As Byte, Optional b As Variant ) As Variant
    Fz_U63 = IIf(a, b, -b)
End Function
Public Function Fz_U64(ByVal a As Double, Optional ByVal b As String = 2) As Date
    Fz_U64 = a & b
End Function
Public Function Fz_U65(ByVal a As Boolean, Optional ByVal b As String = 2) As Currency
    Fz_U65 = Int(a) - Fix(b)
End Function
Public Function Fz_U66(ByVal a As Byte, Optional ByVal b As Date = 2) As Boolean
    On Error Resume Next
    Fz_U66 = Int(a) - Fix(b)
End Function
Public Function Fz_U67(ByVal a As Long, Optional ByVal b As Double = 2) As Currency
    Fz_U67 = Int(a) - Fix(b)
End Function
Public Function Fz_U68(ByVal a As Boolean, Optional ByVal b As Boolean = 2) As Integer
    Fz_U68 = Array(a, b)
End Function
Public Function Fz_U69(ByVal a As Currency, Optional ByVal b As Date = 2) As Date
    Fz_U69 = Val(a)
End Function
