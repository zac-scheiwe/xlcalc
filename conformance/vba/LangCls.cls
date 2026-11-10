VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "LangCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Event Changed(ByVal n As Long, ByRef Cancel As Boolean)
Public Event Plain()
Private m_v As Variant
Private m_n As Long
Private m_log As String
Public Tag As String
Public Counter As Long
Private Static_dummy As Long

Private Sub Class_Initialize()
    L2Log = L2Log & "I"
    m_n = 10
End Sub

Private Sub Class_Terminate()
    L2Log = L2Log & "T" & Tag
End Sub

Public Property Get V() As Variant
    If IsObject(m_v) Then
        Set V = m_v
    Else
        V = m_v
    End If
End Property

Public Property Let V(ByVal x As Variant)
    m_v = x
End Property

Public Property Set V(ByVal x As Object)
    Set m_v = x
End Property

Public Property Get N() As Long
    N = m_n
End Property

Public Property Let N(ByVal x As Long)
    Dim cancel As Boolean
    RaiseEvent Changed(x, cancel)
    If Not cancel Then m_n = x
End Property

Public Property Get Item(ByVal i As Long) As String
    Item = "i" & i
End Property

Public Property Let Item(ByVal i As Long, ByVal s As String)
    L2Log = L2Log & "[" & i & s & "]"
End Property

Public Property Get RO() As Long
    RO = 42
End Property

Public Function Twice(ByVal x As Long) As Long
    Twice = x * 2
End Function

Public Sub Bump(Optional ByVal by As Long = 1)
    m_n = m_n + by
    Counter = Counter + 1
End Sub

Public Function Self() As LangCls
    Set Self = Me
End Function

Public Function Count2() As Long
    Static c As Long
    c = c + 1
    Count2 = c
End Function

Public Sub Fire()
    RaiseEvent Plain
End Sub

Public Sub ByRefSet(ByRef x As Long)
    x = x + 100
End Sub
