import mongoose from 'mongoose';
import {IBridgePersistence} from "../../dataschema/IBridgePersistence";

const BridgeSchema = new mongoose.Schema(
  {
    domainId: { type: String, unique: true },
    name: { type: String, unique: false },
    code: { type: String, unique: false },
    floorA: { type: String, unique: false },
    floorB: { type: String, unique: false },
    //bridgedFloors: { type: [String], unique: true },
  },
  {
    timestamps: true
  }
);

export default mongoose.model<IBridgePersistence & mongoose.Document>('Bridge', BridgeSchema);
