VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "VxSrc"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Event Ping(ByVal n As Long)
Public Event Veto(ByRef Cancel As Boolean)
Public Event Modify(ByRef s As String)
Public Event Many(ByVal a As Long, ByVal b As String, ByVal c As Double)

Private Sub Class_Terminate()
    XLog.Lg "src term"
End Sub

Public Function Raise1(ByVal n As Long) As String
    XLog.Lg "before ping"
    RaiseEvent Ping(n)
    XLog.Lg "after ping"
    Raise1 = "r" & n
End Function

Public Function RaiseVeto() As Boolean
    Dim c As Boolean
    RaiseEvent Veto(c)
    RaiseVeto = c
End Function

Public Function RaiseMod(ByVal s As String) As String
    RaiseEvent Modify(s)
    RaiseMod = s
End Function

Public Sub RaiseMany()
    RaiseEvent Many(1, "two", 3.5)
End Sub
