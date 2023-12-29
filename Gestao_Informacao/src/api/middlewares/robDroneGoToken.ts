import {JwtPayload} from "jwt-decode";

export interface RobDroneGoToken extends JwtPayload {
  email?:string;
  given_name?:string;
  family_name?:string;
  role?:string;
}
