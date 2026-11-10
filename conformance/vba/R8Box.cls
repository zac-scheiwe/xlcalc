VERSION 1.0 CLASS
BEGIN
  MultiUse = -1
END
Attribute VB_Name = "R8Box"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public Event Changed(ByVal n As Long, ByRef Cancel As Boolean)
Public Event Plain()
Public Event Msg(ByRef s As String)
Private m_n As Long
Private m_v As Variant
Private m_w As Long
Public Tag As String
Public Hits As Long

Private Sub Class_Initialize()
    R8Log = R8Log & "I"
    m_n = 10
End Sub

Private Sub Class_Terminate()
    R8Log = R8Log & "T" & Tag
End Sub

Public Property Get N() As Long
    N = m_n
End Property

Public Property Let N(ByVal x As Long)
    Dim c As Boolean
    RaiseEvent Changed(x, c)
    If Not c Then m_n = x
End Property

Public Property Get V() As Variant
    If IsObject(m_v) Then
        Set V = m_v
    Else
        V = m_v
    End If
End Property

Public Property Let V(ByVal x As Variant)
    R8Log = R8Log & "[Let" & TypeName(x) & "]"
    m_v = x
End Property

Public Property Set V(ByVal x As Object)
    R8Log = R8Log & "[Set]"
    Set m_v = x
End Property

Public Property Get RO() As Long
    RO = 42
End Property

Public Property Let WO(ByVal x As Long)
    m_w = x
End Property

Public Function PeekWO() As Long
    PeekWO = m_w
End Function

Public Property Get Item(ByVal i As Long) As String
    Item = "i" & i
End Property

Public Property Let Item(ByVal i As Long, ByVal s As String)
    R8Log = R8Log & "[" & i & s & "]"
End Property

Public Property Get Dflt() As Long
Attribute Dflt.VB_UserMemId = 0
    Dflt = m_n * 2
End Property

Public Property Let Dflt(ByVal x As Long)
    m_n = x + 1
End Property

Public Function Twice(ByVal x As Long) As Long
    Twice = x * 2
End Function

Public Sub Bump(Optional ByVal by As Long = 1)
    m_n = m_n + by
    Hits = Hits + 1
End Sub

Public Function Self() As R8Box
    Set Self = Me
End Function

Public Function Cnt() As Long
    Static c As Long
    c = c + 1
    Cnt = c
End Function

Public Sub Fire()
    RaiseEvent Plain
End Sub

Public Function FireMsg(ByVal s As String) As String
    RaiseEvent Msg(s)
    FireMsg = s
End Function

Public Sub Boom(ByVal n As Long)
    Err.Raise n, "R8Box.Boom", "boom" & n
End Sub

Public Sub ByRefSet(ByRef x As Long)
    x = x + 100
End Sub

Public Function SumAll(ParamArray p() As Variant) As Double
    Dim i As Long
    For i = LBound(p) To UBound(p)
        SumAll = SumAll + p(i)
    Next i
End Function
