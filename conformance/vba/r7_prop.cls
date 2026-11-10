VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "VxProp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private mV As Variant
Private mO As Object
Private mArr(1 To 3) As String

Public Property Get Value() As Variant
Attribute Value.VB_UserMemId = 0
    If IsObject(mV) Then
        Set Value = mV
    Else
        Value = mV
    End If
End Property

Public Property Let Value(ByVal x As Variant)
    XLog.Lg "let value " & TypeName(x)
    mV = x
End Property

Public Property Set Value(ByVal x As Object)
    XLog.Lg "set value " & TypeName(x)
    Set mV = x
End Property

Public Property Get Obj() As Object
    Set Obj = mO
End Property

Public Property Set Obj(ByVal x As Object)
    Set mO = x
End Property

Public Property Get Arr(ByVal i As Long) As String
    Arr = mArr(i)
End Property

Public Property Let Arr(ByVal i As Long, ByVal s As String)
    mArr(i) = s
End Property

Public Property Get WriteOnly() As Long
    Err.Raise 5, , "no get"
End Property

Public Property Let WriteOnly(ByVal x As Long)
    XLog.Lg "wo " & x
End Property

Friend Function Fr(ByVal x As Long) As Long
    Fr = x + 1
End Function

Friend Property Get FrP() As String
    FrP = "frp"
End Property

Private Function Priv() As String
    Priv = "priv"
End Function

Public Function CallPriv() As String
    CallPriv = Priv()
End Function

Public Function Opt(Optional ByVal a As Long = 5, Optional ByVal b As Variant) As String
    Opt = a & "," & IsMissing(b)
End Function

Public Function PArr(ParamArray p() As Variant) As String
    PArr = UBound(p) - LBound(p) + 1 & ":" & TypeName(p)
End Function

Public Function MeSelf() As Object
    Set MeSelf = Me
End Function

Public Property Get ReadOnly1() As Long
    ReadOnly1 = 99
End Property
