export interface RobotDTO {
  id: string;
  nickName: string;
  robotType: string;
  serialNumber: string;
  description?: string;
  inhibited: boolean;
}

export interface RobotResponseDTO {
  nickName: string;
  robotType: string;
  serialNumber: string;
  description?: string;
  inhibited: boolean;
}