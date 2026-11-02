VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "ClsHolder"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Prop As Object

Public Property Set Target(ByVal o As Object)
    On Error GoTo EH
    Set Prop = o
    Exit Property
EH:
    VcLog.Lg "set error " & Err.Number
End Property

Public Property Get Target() As Object
    On Error GoTo EH
    Set Target = Prop
    Exit Property
EH:
    VcLog.Lg "get error " & Err.Number
End Property

Private Sub Class_Terminate()
    On Error GoTo EH
    VcLog.Lg "holder term"
    Exit Sub
EH:
    VcLog.Lg "holder term error " & Err.Number
End Sub
