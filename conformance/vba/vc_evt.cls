VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "ClsEvt"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Event Fired(ByVal n As Long, ByRef Cancel As Boolean)

Public Function Fire(ByVal n As Long) As Boolean
    On Error GoTo EH
    Dim c As Boolean
    RaiseEvent Fired(n, c)
    Fire = c
    Exit Function
EH:
    VcLog.Lg "fire error " & Err.Number
End Function
