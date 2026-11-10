VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "VxSq"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Implements VxShape
Implements VxNamed
Public Side As Double
Public Tag As String
Private mNote As String

Private Sub Class_Initialize()
    XLog.Lg "sq init"
End Sub

Private Sub Class_Terminate()
    XLog.Lg "sq term " & Tag
End Sub

Private Function VxShape_Area() As Double
    VxShape_Area = Side * Side
End Function

Private Function VxShape_Label() As String
    VxShape_Label = "shape-label"
End Function

Private Property Get VxShape_Id() As Long
    VxShape_Id = 7
End Property

Private Property Let VxShape_Note(ByVal s As String)
    mNote = s
End Property

Private Sub VxShape_Draw(Optional ByVal times As Long = 1)
    XLog.Lg "draw x" & times & " note=" & mNote
End Sub

Private Function VxNamed_Label() As String
    VxNamed_Label = "named-label"
End Function

Public Function Label() As String
    Label = "own-label"
End Function

Public Property Get Note() As String
    Note = mNote
End Property
