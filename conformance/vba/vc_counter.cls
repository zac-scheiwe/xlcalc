VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "ClsCounter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private n As Long
Public Name As String

Private Sub Class_Initialize()
    On Error GoTo EH
    VcLog.Lg "init"
    n = 10
    Exit Sub
EH:
    VcLog.Lg "init error " & Err.Number
End Sub

Private Sub Class_Terminate()
    On Error GoTo EH
    VcLog.Lg "term " & Name
    Exit Sub
EH:
    VcLog.Lg "term error " & Err.Number
End Sub

Public Property Get Count() As Long
    On Error GoTo EH
    Count = n
    Exit Property
EH:
    Count = -1
End Property

Public Property Let Count(ByVal v As Long)
    On Error GoTo EH
    n = v
    Exit Property
EH:
    VcLog.Lg "let error " & Err.Number
End Property

Public Property Get Item() As Variant
Attribute Item.VB_UserMemId = 0
    On Error GoTo EH
    Item = "default:" & n
    Exit Property
EH:
    Item = "err " & Err.Number
End Property

Public Sub Inc()
    On Error GoTo EH
    n = n + 1
    Exit Sub
EH:
    VcLog.Lg "inc error " & Err.Number
End Sub

Public Function Self() As ClsCounter
    On Error GoTo EH
    Set Self = Me
    Exit Function
EH:
    VcLog.Lg "self error " & Err.Number
End Function
