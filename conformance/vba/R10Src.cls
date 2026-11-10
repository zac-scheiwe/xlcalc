VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R10Src"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Event Ev1(ByVal n As Long, ByRef res As String)
Public Event Ev2()
Public Tag As String

Public Function Fire1(ByVal n As Long) As String
    Dim r As String
    r = "r"
    On Error GoTo EH
    RaiseEvent Ev1(n, r)
    Fire1 = r
    Exit Function
EH:
    Fire1 = r & "!" & Err.Number & ":" & Err.Description
End Function

Public Function Fire1U(ByVal n As Long) As String
    Dim r As String
    r = "r"
    RaiseEvent Ev1(n, r)
    Fire1U = r
End Function

Public Sub Fire2()
    R10Log = R10Log & "<a>"
    RaiseEvent Ev2
    R10Log = R10Log & "<b>"
End Sub
