import { Repo } from "../../core/infra/Repo";
import { Role } from "../../domain/role-agg/role";
import { RoleId } from "../../domain/role-agg/roleId";

export default interface IRoleRepo extends Repo<Role> {
  save(role: Role): Promise<Role>;
  findByDomainId (roleId: RoleId | string): Promise<Role>;

  //findByIds (rolesIds: RoleId[]): Promise<Role[]>;
  //saveCollection (roles: Role[]): Promise<Role[]>;
  //removeByRoleIds (roles: RoleId[]): Promise<any>
}
