import { Repo } from "../../core/infra/Repo";

import { User } from "../../domain/user";
import { UserEmail } from "../../domain/userEmail";

export interface IUserRepo extends Repo<User> {
	findByEmail (email: UserEmail | string): Promise<User>;
	save(user: User): Promise<User>;
  }
  