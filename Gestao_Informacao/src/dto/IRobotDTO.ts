export default interface IRobotDTO {
  id: string;
  nickName: string;
  robotType: string;
  serialNumber: string;
  description?: string;
  inhibited: boolean;
}
