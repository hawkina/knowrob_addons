/** <module> openease_sherpa

  Copyright (C) 2017 Asil Kaan Bozcuoglu
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the <organization> nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  @author Asil Kaan Bozcuoglu
  @license BSD
*/

:- module(openease_sherpa,
    [
        publish_marker_id/1,
        publish_command/1,
        save_canvas_latest/3,
        check_object_inside_polygon/2,
        transform_latest_z_axis_down/3
    ]).
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('owl')).
:- use_module(library('rdfs_computable')).
:- use_module(library('owl_parser')).
:- use_module(library('comp_temporal')).
:- use_module(library('knowrob_mongo')).
:- use_module(library('srdl2')).
:- use_module(library('lists')).

:- rdf_db:rdf_register_ns(knowrob, 'http://knowrob.org/kb/knowrob.owl#',  [keep(true)]).
:- rdf_db:rdf_register_ns(knowrob_cram, 'http://knowrob.org/kb/knowrob_cram.owl#', [keep(true)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- assert(shr_interface(fail)).

sherpa_interface :- sherpa_interface(_).

sherpa_interface(S) :-
    shr_interface(fail),
    jpl_new('org.knowrob.sherpa.HMIPublisher', [], S),
    jpl_list_to_array(['org.knowrob.sherpa.HMIPublisher'], Arr),
    jpl_call('org.knowrob.utils.ros.RosUtilities', runRosjavaNode, [S, Arr], _),
    retract(shr_interface(fail)),
    assert(shr_interface(S)),!.
sherpa_interface(S) :-
    shr_interface(S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publish_marker_id(Marker) :-
    sherpa_interface(S),
    term_to_atom( Marker, MarkerAtom),
    jpl_call(S, 'publishMarker', [MarkerAtom], _).


publish_command(Cmd) :-
    sherpa_interface(S),
    term_to_atom( Cmd, CmdAtom),
    jpl_call(S, 'publishCommand', [CmdAtom], _).


save_canvas_latest(Base64, Path, CompletePath) :-
  sherpa_interface(S),
  jpl_call(S, 'saveCanvasLatest', [Base64, Path], CompletePath).

check_object_inside_polygon(Object, Polygon) :-
  object_dimensions(Object, ObjDepth, ObjWidth, ObjHeight),
  object_dimensions(Polygon, PlDepth, PlWidth, PlHeight),
  current_object_pose(Object, [ObjX,ObjY,ObjZ,ObjQW,ObjQX,ObjQY,ObjQZ]),
  current_object_pose(Polygon, [PlX,PlY,PlZ,PlQW,PlQX,PlQY,PlQZ]),

  >=( (ObjX - 0.5*ObjDepth), (PlX - 0.5*PlDepth)-0.05), =<( (ObjX + 0.5*ObjDepth),  (PlX + 0.5*PlDepth)+0.05 ),
  >=( (ObjY - 0.5*ObjWidth), (PlY - 0.5*PlWidth)-0.05 ), =<( (ObjY + 0.5*ObjWidth), (PlY + 0.5*PlWidth)+0.05 ),
  >=( (ObjZ - 0.5*ObjHeight), (PlZ - 0.5*PlHeight)-0.05 ), =<( (ObjZ + 0.5*ObjHeight), (PlZ + 0.5*PlHeight)+0.05 ).

  %owl_individual_of(ObjectPerc, knowrob:'SemanticMapPerception'),
  %rdf_has(ObjectPerc, knowrob:'objectActedOn', Object),
  
transform_latest_z_axis_down(Target, SourceFrame, Out) :-
  mongo_interface(DB),
  jpl_call(DB, 'lookupTransform', [Target, SourceFrame], StampedTransform),
  % Make sure transform is not null!
  not( jpl_null(StampedTransform) ),

  TimeInt is 0,

  Z = [-1,0,0,0,0,1,0,0,0,0,-1,-1,0,0,0,1],
  knowrob_coordinates:list_to_matrix4d(Z, Z_Matrix),
  jpl_new('tfjava.Stamped', [Z_Matrix, SourceFrame, TimeInt], Z_Stamped),

  knowrob_coordinates:list_to_matrix4d([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1], MatrixOut),
  % create intermediate matrix 
  jpl_new('tfjava.Stamped', [MatrixOut, '/', TimeInt], Out_Stamped),

  jpl_call(Z_Stamped, 'getData', [], Z_Data),
  jpl_call(Out_Stamped, 'getData', [], Out_Data),

  jpl_call(StampedTransform, 'transformPose', [Z_Data, Out_Data], _),

  jpl_call(Out_Stamped, 'getData', [], TransformMatrix4d),
  knowrob_coordinates:matrix4d_to_list(TransformMatrix4d, Out).  

