import mongoose from "mongoose";
import {IRoomPersistence} from "../../dataschema/IRoomPersistence";

const RoomSchema = new mongoose.Schema({
    domainId: { type: String, unique: true },
    buildingId: {type: String, unique: false},
    floorId: {type: String, unique: false},
    name: {type: String, unique: true},
    roomType:  {type: String, unique:false},
    description: {type: String, unique: false}
  },
  {
    timestamps: true
  }
);
export default mongoose.model<IRoomPersistence & mongoose.Document>('Room', RoomSchema);
