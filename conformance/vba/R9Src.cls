VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R9Src"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event Ev1(ByVal n As Long, ByRef res As String)
Public Event Ev2(ByRef a As Long, ByRef b As Variant)
Public Event Ev3()
Public Tag As String

Private Sub Class_Initialize()
    R9Log = R9Log & "I"
End Sub

Private Sub Class_Terminate()
    R9Log = R9Log & "T" & Tag
End Sub

Public Function Fire1(ByVal n As Long) As String
    Dim r As String
    r = "r"
    RaiseEvent Ev1(n, r)
    Fire1 = r
End Function

Public Function Fire2(ByRef a As Long, ByRef b As Variant) As String
    RaiseEvent Ev2(a, b)
    Fire2 = a & TypeName(b) & b
End Function

Public Sub Fire3()
    RaiseEvent Ev3
End Sub

Public Function FireLoc(ByVal n As Long) As Long
    Dim k As Long
    Dim v As Variant
    k = n: v = n
    RaiseEvent Ev2(k, v)
    FireLoc = k
End Function
