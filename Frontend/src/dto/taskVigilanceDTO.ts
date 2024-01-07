
export interface TaskVigilanceRequestDTO {
  id: string;
  description: string;
  buildingId: string;
  floors: object[];
  contactNumber: number;
  user:object;
  approved: boolean;
  pending: boolean;
  planned :boolean;
}


export interface TaskVigilanceResponseDTO {
  id: string;
  description: string;
  buildingId: string;
  floors: object[];
  contactNumber: number;
  user:object;
  approved: boolean;
  pending: boolean;
  planned :boolean;
}

export interface TaskVigilancePedidoDTO {
  id: string;
  description: string;
  buildingId: string;
  floors: object[];
  contactNumber: number;
  user:object;
  approved: boolean;
  pending: boolean;
  planned :boolean;
  endPosition: number[];
  startPosition: number[];
}