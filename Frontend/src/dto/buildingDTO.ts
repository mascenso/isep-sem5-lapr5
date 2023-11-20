export interface BuildingResponseDTO {
  id: string;
  code: string;
  maxWidth: string,
  maxLength: string,
  name: string;
  description: string;
}

export interface CreateBuildingRequestDTO {
  code: string;
  maxWidth: string,
  maxLength: string,
  name?: string;
  description?: string;
}
