export interface RoomResponseDTO {
  name: string,
  description: string,
  roomType: string,
}

export interface RoomRequestDTO {

  id: string,
  buildingId:string,
  floorId: string,
  name:string,
  roomType:string,
  description: string,

}



